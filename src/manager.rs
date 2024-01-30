use std::collections::HashMap;

use anyhow::anyhow;
use log::*;
use lsp_server::{Connection, Message, RequestId, ResponseError};
use lsp_types::{
    notification::Notification,
    request::{Request, Shutdown},
    ClientCapabilities,
};
use serde::Serialize;

use crate::json_rpc_errors;

type ResponseHandler<S> =
    Box<dyn FnOnce(&mut State<S>, lsp_server::Response) -> anyhow::Result<()>>;
pub const NULL_NONE: Option<()> = None;

pub struct State<S> {
    requests: Vec<(&'static str, Box<dyn Handle<S, lsp_server::Request>>)>,
    notifications: Vec<(&'static str, Box<dyn Handle<S, lsp_server::Notification>>)>,
    response: Vec<(RequestId, ResponseHandler<S>)>,

    channel: Connection,
    next_id: i32,

    client: ClientCapabilities,
    local: S,
}

trait Handle<S, T> {
    fn handle(&mut self, state: &mut State<S>, v: T) -> anyhow::Result<()>;
}
impl<F, S, T> Handle<S, T> for F
where
    F: FnMut(&mut State<S>, T) -> anyhow::Result<()>,
{
    fn handle(&mut self, state: &mut State<S>, v: T) -> anyhow::Result<()> {
        self(state, v)
    }
}

pub struct Manager<S> {
    state: State<S>,
    requests: HashMap<&'static str, Box<dyn Handle<S, lsp_server::Request>>>,
    notifications: HashMap<&'static str, Box<dyn Handle<S, lsp_server::Notification>>>,
    response: HashMap<RequestId, ResponseHandler<S>>,
}

impl<S> Manager<S> {
    pub fn new(channel: Connection, client: ClientCapabilities, local: S) -> Self {
        Self {
            state: State {
                requests: vec![],
                notifications: vec![],
                response: vec![],

                channel,
                next_id: 0,
                client,
                local,
            },
            requests: HashMap::new(),
            notifications: HashMap::new(),
            response: HashMap::new(),
        }
    }

    pub fn state(&mut self) -> &mut State<S> {
        &mut self.state
    }

    fn apply(&mut self) {
        self.requests.extend(self.state.requests.drain(..));
        self.notifications
            .extend(self.state.notifications.drain(..));
        self.response.extend(self.state.response.drain(..));
    }

    pub fn run(mut self) -> anyhow::Result<()> {
        self.apply();
        let mut shutdown = false;
        while let Ok(msg) = self.state.channel.receiver.recv() {
            match msg {
                Message::Request(req) => {
                    debug!("Request: {}", req.method);
                    debug!("{:?}", req.params);
                    if shutdown {
                        self.state.error(
                            req.id,
                            json_rpc_errors::INVALID_REQUEST,
                            "Invalid Request".into(),
                            Option::<()>::None,
                        )?;
                    } else {
                        if req.method == "shutdown" {
                            self.state.respond::<Shutdown>(req.id, ())?;
                            shutdown = true;
                        } else {
                            if let Some(handler) = self.requests.get_mut(req.method.as_str()) {
                                handler.handle(&mut self.state, req)?;
                            } else {
                                self.state.error(
                                    req.id,
                                    json_rpc_errors::METHOD_NOT_FOUND,
                                    format!("{} is not recognized", req.method),
                                    NULL_NONE,
                                )?;
                            }
                        }
                    }
                }
                Message::Response(res) => error!("res {:?}", res),
                Message::Notification(n) => {
                    debug!("Noftification: {}", n.method);
                    debug!("{:?}", n.params);
                    if n.method == "exit" {
                        break;
                    } else {
                        if let Some(handler) = self.notifications.get_mut(n.method.as_str()) {
                            handler.handle(&mut self.state, n)?;
                        }
                        // Unknown notifcations are ignored
                    }
                }
            }
            self.apply();
        }
        Ok(())
    }
}

impl<S> State<S> {
    pub fn on_request<R: Request>(
        &mut self,
        mut handle: impl FnMut(&mut State<S>, R::Params) -> Result<R::Result, ResponseError> + 'static,
    ) -> &mut Self {
        self.requests.push((
            R::METHOD,
            Box::new(
                move |state: &mut State<S>,
                      req: lsp_server::Request|
                      -> Result<(), anyhow::Error> {
                    match serde_json::from_value(req.params) {
                        Ok(v) => match handle(state, v) {
                            Ok(v) => state.respond::<R>(req.id, v),
                            Err(e) => state.error(req.id, e.code, e.message, e.data),
                        },
                        Err(e) => state.error(
                            req.id,
                            json_rpc_errors::INVALID_REQUEST,
                            format!("{:?}", e),
                            NULL_NONE,
                        ),
                    }
                },
            ),
        ));
        self
    }

    pub fn on_notification<N: Notification>(
        &mut self,
        mut handle: impl FnMut(&mut State<S>, N::Params) -> anyhow::Result<()> + 'static,
    ) -> &mut Self {
        self.notifications.push((
            N::METHOD,
            Box::new(
                move |state: &mut State<S>, not: lsp_server::Notification| -> anyhow::Result<()> {
                    if let Ok(v) = serde_json::from_value(not.params) {
                        if let Err(e) = handle(state, v) {
                            error!("{}", e);
                        }
                    }
                    Ok(())
                },
            ),
        ));
        self
    }

    pub fn request<R: Request>(
        &mut self,
        value: R::Params,
        handle: impl FnOnce(&mut State<S>, R::Result) -> anyhow::Result<()> + 'static,
    ) -> anyhow::Result<()> {
        self.channel
            .sender
            .send(Message::Request(lsp_server::Request {
                id: self.next_id.into(),
                method: R::METHOD.to_owned(),
                params: serde_json::to_value(value)?,
            }))?;
        self.response.push((
            self.next_id.into(),
            Box::new(
                move |state, response| match (response.result, response.error) {
                    (Some(value), None) => {
                        let response: R::Result = serde_json::from_value(value)?;
                        handle(state, response)
                    }
                    (None, Some(e)) => Err(anyhow!("Client error: {:?}", e)),
                    (None, None) => todo!("It's unclear if this is reachable"),
                    (Some(_), Some(_)) => todo!("It's unclear if this is reachable"),
                },
            ),
        ));
        self.next_id += 1;
        Ok(())
    }

    fn respond<R: Request>(&mut self, to: RequestId, value: R::Result) -> anyhow::Result<()> {
        self.channel
            .sender
            .send(Message::Response(lsp_server::Response {
                id: to,
                result: Some(serde_json::to_value(value)?),
                error: None,
            }))
            .map_err(|e| e.into())
    }

    fn error(
        &mut self,
        to: RequestId,
        code: i32,
        message: String,
        value: Option<impl Serialize>,
    ) -> anyhow::Result<()> {
        self.channel
            .sender
            .send(Message::Response(lsp_server::Response {
                id: to,
                error: Some(ResponseError {
                    code,
                    message,
                    data: value.map(|value| serde_json::to_value(value)).transpose()?,
                }),
                result: None,
            }))
            .map_err(|e| e.into())
    }

    pub fn notify<N: Notification>(&mut self, value: N::Params) -> anyhow::Result<()> {
        self.channel
            .sender
            .send(Message::Notification(lsp_server::Notification {
                method: N::METHOD.to_owned(),
                params: serde_json::to_value(value)?,
            }))
            .map_err(|e| e.into())
    }

    pub fn local(&mut self) -> &mut S {
        &mut self.local
    }
}
