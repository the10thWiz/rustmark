mod manager;
mod markdown;

use std::collections::HashMap;

use anyhow::anyhow;
use json_rpc_errors::param_error;
use log::*;
use lsp_server::{Connection, ResponseError};
use lsp_types::{
    notification::*, request::*, ClientCapabilities, FormattingOptions, InitializeParams,
    InlayHint, InlayHintOptions, Location, OneOf, Position, Range, ServerCapabilities,
    SymbolInformation, SymbolKind, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions, TextEdit, Url, WorkDoneProgressOptions,
};
use manager::Manager;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Deserialize, Serialize)]
struct InitParams {}

fn main() {
    let f = std::fs::File::create("rustmark.log").unwrap();
    env_logger::builder()
        .target(env_logger::Target::Pipe(Box::new(f)))
        .init();
    std::panic::set_hook(Box::new(|info| {
        error!("Panic: {}", info);
    }));
    match run() {
        Ok(()) => (),
        Err(e) => {
            error!("Error occured: {}", e);
            error!("{}", e.backtrace());
        }
    }
}

fn run() -> anyhow::Result<()> {
    let (conn, _io_threads) = Connection::stdio();
    let (id, params) = conn.initialize_start()?;
    let init_params: InitializeParams = serde_json::from_value(params)?;
    let client_cababilities = init_params.capabilities;
    let mut server_capabilities = ServerCapabilities::default();
    server_capabilities.text_document_sync = Some(TextDocumentSyncCapability::Options(
        TextDocumentSyncOptions {
            open_close: Some(true),
            change: Some(TextDocumentSyncKind::INCREMENTAL),
            ..Default::default()
        },
    ));
    // server_capabilities.code_lens_provider = Some(lsp_types::CodeLensOptions {
    //     resolve_provider: Some(true),
    // });
    // server_capabilities.document_formatting_provider = Some(OneOf::Left(true));
    // server_capabilities.inlay_hint_provider = Some(OneOf::Left(true));
    server_capabilities.document_symbol_provider =
        Some(OneOf::Right(lsp_types::DocumentSymbolOptions {
            label: Some("RustMark".into()),
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
        }));
    let init_data = serde_json::json!({
        "capabilities": server_capabilities,
        "serverInfo": {
            "name": "rustmark",
            "version": env!("CARGO_PKG_VERSION"),
        }
    });
    conn.initialize_finish(id, init_data)?;
    info!("Server Init");

    // error!("Client: {client_cababilities:?}");

    run_connection(conn, client_cababilities)
}

struct ClientCaps(ClientCapabilities);

impl ClientCaps {
    pub fn apply_edit(&self) -> bool {
        self.0
            .workspace
            .as_ref()
            .map_or(false, |w| w.apply_edit.unwrap_or(false))
    }
}

mod json_rpc_errors {
    use lsp_server::ResponseError;

    // /// Invalid JSON was received by the server.
    // /// An error occurred on the server while parsing the JSON text.
    // pub const PARSE_ERROR: i32 = -32700;
    /// The JSON sent is not a valid Request object.
    pub const INVALID_REQUEST: i32 = -32600;
    /// The method does not exist / is not available.
    pub const METHOD_NOT_FOUND: i32 = -32601;
    /// Invalid method parameter(s).
    pub const INVALID_PARAMS: i32 = -32602;

    pub fn param_error(message: impl Into<String>) -> ResponseError {
        ResponseError {
            code: INVALID_PARAMS,
            message: message.into(),
            data: None,
        }
    }
}

#[derive(Debug)]
struct LspFile {
    contents: String,
}

#[derive(Debug, Default)]
struct Local {
    open_files: HashMap<Url, LspFile>,
}

type Result<T, E = ResponseError> = std::result::Result<T, E>;

impl Local {
    fn file(&self, document: &Url) -> Result<&LspFile> {
        self.open_files
            .get(document)
            .ok_or_else(|| param_error(format!("{} is not open", document)))
    }

    fn file_mut(&mut self, document: &Url) -> Result<&mut LspFile> {
        self.open_files
            .get_mut(document)
            .ok_or_else(|| param_error(format!("{} is not open", document)))
    }
}

/// Computer the offset of string inner, within outer. Note that inner MUST be a substring of outer, since this
/// actually computes the byte offset of the pointers, not locating substrings
fn byte_offset(outer: &str, inner: &str) -> usize {
    assert!(
        outer.as_ptr() as usize <= inner.as_ptr() as usize
            && inner.as_ptr() as usize + inner.len() <= outer.as_ptr() as usize + outer.len(),
        "Inner MUST be a substring of outer"
    );
    inner.as_ptr() as usize - outer.as_ptr() as usize
}

/// Position -> byte offset. Returns None if the position is outside the document
fn byte_position(pos: Position, contents: &str) -> Option<usize> {
    let line = contents.lines().nth(pos.line as usize)?;
    // SAFETY: line is a pointer into contents, so they are trivially within the same object.
    let line_start = byte_offset(contents, line);
    Some(line.char_indices().nth(pos.character as usize)?.0 + (line_start as usize))
}

/// Range -> byte offset range. Returns None if either position is outside the document, or the start is before the end
fn byte_range(range: Range, contents: &str) -> Option<std::ops::Range<usize>> {
    let start = byte_position(range.start, contents)?;
    let end = byte_position(range.end, contents)?;
    if end > start {
        None
    } else {
        Some(start..end)
    }
}

fn run_connection(conn: Connection, client: ClientCapabilities) -> anyhow::Result<()> {
    let mut manager = Manager::new(conn, client, Local::default());
    manager
        .state()
        .on_notification::<DidOpenTextDocument>(|state, params| {
            info!("Opened text file `{}`", params.text_document.uri);
            state.local().open_files.insert(
                params.text_document.uri,
                LspFile {
                    contents: params.text_document.text,
                },
            );
            Ok(())
        })
        .on_notification::<DidChangeTextDocument>(|state, params| {
            info!("Changed text file `{}`", params.text_document.uri);
            if let Ok(file) = state.local().file_mut(&params.text_document.uri) {
                for change in params.content_changes {
                    if let Some(range) = change.range {
                        let range = byte_range(range, file.contents.as_str())
                            .ok_or_else(|| anyhow!("Byte range out of bounds"))?;
                        file.contents.replace_range(range, &change.text);
                    } else {
                        file.contents = change.text;
                    }
                }
            }
            Ok(())
        })
        .on_notification::<DidCloseTextDocument>(|state, params| {
            info!("Closed text file `{}`", params.text_document.uri);
            state.local().open_files.remove(&params.text_document.uri);
            Ok(())
        })
        // .on_request::<CodeLensRequest>(|state, params| {
        //     let v = state
        //         .local()
        //         .open_files
        //         .get(&params.text_document.uri)
        //         .ok_or_else(|| lsp_server::ResponseError {
        //             code: json_rpc_errors::INVALID_PARAMS,
        //             message: format!("Text document not open"),
        //             data: None,
        //         })?;
        //     Ok(vec![])
        // })
        .on_request::<Formatting>(|state, params| {
            let file = state
                .local()
                .open_files
                .get(&params.text_document.uri)
                .ok_or_else(|| param_error("File not open"))?;
            format_file(file, params.options).map(|v| Some(v))
        })
        .on_request::<InlayHintRequest>(|state, params| {
            let file = state.local().file(&params.text_document.uri)?;
            Ok(Some(vec![]))
        })
        .on_request::<DocumentSymbolRequest>(|state, params| {
            let file = state.local().file(&params.text_document.uri)?;
            let mut ret = vec![];
            for (i, line) in file.contents.lines().enumerate() {
                if line.starts_with("#") {
                    let val = line.trim_start_matches("#");
                    ret.push(SymbolInformation {
                        name: val.to_owned(),
                        kind: SymbolKind::MODULE,
                        tags: None, // TODO
                        location: Location {
                            uri: params.text_document.uri.clone(),
                            range: Range {
                                start: Position {
                                    line: i as u32,
                                    character: byte_offset(line, val) as u32,
                                },
                                end: Position {
                                    line: i as u32,
                                    character: line.len() as u32,
                                },
                            },
                        },
                        container_name: None,
                        deprecated: None,
                    });
                }
            }
            Ok(Some(lsp_types::DocumentSymbolResponse::Flat(ret)))
        });
    manager.run()
}

fn format_file(file: &LspFile, opts: FormattingOptions) -> Result<Vec<TextEdit>, ResponseError> {
    // Step 1: parse file
    // Step 2: write file to string
    // Maybe just a single edit? it's not ideal, but it makes this much easier
    todo!()
}

#[cfg(test)]
mod tests {
    use std::thread::{self, JoinHandle};

    use lsp_types::{
        CodeActionCapabilityResolveSupport, CodeActionClientCapabilities,
        CodeActionKindLiteralSupport, CodeActionLiteralSupport, CompletionClientCapabilities,
        CompletionItemCapability, CompletionItemCapabilityResolveSupport,
        CompletionItemKindCapability, CompletionItemTag, DidChangeWatchedFilesClientCapabilities,
        DynamicRegistrationClientCapabilities, FailureHandlingKind, GeneralClientCapabilities,
        HoverClientCapabilities, InlayHintClientCapabilities, InlayHintWorkspaceClientCapabilities,
        MarkupKind, ParameterInformationSettings, PositionEncodingKind,
        PublishDiagnosticsClientCapabilities, RenameClientCapabilities, ResourceOperationKind,
        SignatureHelpClientCapabilities, SignatureInformationSettings, TagSupport,
        TextDocumentChangeRegistrationOptions, TextDocumentClientCapabilities,
        TextDocumentSyncClientCapabilities, WindowClientCapabilities, WorkspaceClientCapabilities,
        WorkspaceEditClientCapabilities, WorkspaceSymbolClientCapabilities,
    };

    use super::*;

    fn start_server(client: ClientCapabilities) -> (Connection, JoinHandle<anyhow::Result<()>>) {
        let (a, b) = Connection::memory();
        let handle = thread::spawn(move || run_connection(a, client));
        (b, handle)
    }

    fn helix_caps() -> ClientCapabilities {
        ClientCapabilities {
            workspace: Some(WorkspaceClientCapabilities {
                apply_edit: Some(true),
                workspace_edit: Some(WorkspaceEditClientCapabilities {
                    document_changes: Some(true),
                    resource_operations: Some(vec![
                        ResourceOperationKind::Create,
                        ResourceOperationKind::Rename,
                        ResourceOperationKind::Delete,
                    ]),
                    failure_handling: Some(FailureHandlingKind::Abort),
                    normalizes_line_endings: Some(false),
                    change_annotation_support: None,
                }),
                did_change_configuration: Some(DynamicRegistrationClientCapabilities {
                    dynamic_registration: Some(false),
                }),
                did_change_watched_files: Some(DidChangeWatchedFilesClientCapabilities {
                    dynamic_registration: Some(true),
                    relative_pattern_support: Some(false),
                }),
                symbol: Some(WorkspaceSymbolClientCapabilities {
                    dynamic_registration: Some(false),
                    symbol_kind: None,
                    tag_support: None,
                    resolve_support: None,
                }),
                execute_command: Some(DynamicRegistrationClientCapabilities {
                    dynamic_registration: Some(false),
                }),
                workspace_folders: Some(true),
                configuration: Some(true),
                semantic_tokens: None,
                code_lens: None,
                file_operations: None,
                inline_value: None,
                inlay_hint: Some(InlayHintWorkspaceClientCapabilities {
                    refresh_support: Some(false),
                }),
                diagnostic: None,
            }),
            text_document: Some(TextDocumentClientCapabilities {
                synchronization: None,
                completion: Some(CompletionClientCapabilities {
                    dynamic_registration: None,
                    completion_item: Some(CompletionItemCapability {
                        snippet_support: Some(true),
                        commit_characters_support: None,
                        documentation_format: None,
                        deprecated_support: Some(true),
                        preselect_support: None,
                        tag_support: Some(TagSupport {
                            // value_set: vec![CompletionItemTag::Deprecated],
                            value_set: vec![],
                        }),
                        insert_replace_support: Some(true),
                        resolve_support: Some(CompletionItemCapabilityResolveSupport {
                            properties: ["documentation", "detail", "additionalTextEdits"]
                                .into_iter()
                                .map(|s| s.into())
                                .collect(),
                        }),
                        insert_text_mode_support: None,
                        label_details_support: None,
                    }),
                    completion_item_kind: Some(CompletionItemKindCapability { value_set: None }),
                    context_support: None,
                    insert_text_mode: None,
                    completion_list: None,
                }),
                hover: Some(HoverClientCapabilities {
                    dynamic_registration: None,
                    content_format: Some(vec![MarkupKind::Markdown]),
                }),
                signature_help: Some(SignatureHelpClientCapabilities {
                    dynamic_registration: None,
                    signature_information: Some(SignatureInformationSettings {
                        documentation_format: Some(vec![MarkupKind::Markdown]),
                        parameter_information: Some(ParameterInformationSettings {
                            label_offset_support: Some(true),
                        }),
                        active_parameter_support: Some(true),
                    }),
                    context_support: None,
                }),
                references: None,
                document_highlight: None,
                document_symbol: None,
                formatting: None,
                range_formatting: None,
                on_type_formatting: None,
                declaration: None,
                definition: None,
                type_definition: None,
                implementation: None,
                code_action: Some(CodeActionClientCapabilities {
                    dynamic_registration: None,
                    code_action_literal_support: Some(CodeActionLiteralSupport {
                        code_action_kind: CodeActionKindLiteralSupport {
                            value_set: [
                                "",
                                "quickfix",
                                "refactor",
                                "refactor.extract",
                                "refactor.inline",
                                "refactor.rewrite",
                                "source",
                                "source.organizeImports",
                            ]
                            .into_iter()
                            .map(|s| s.into())
                            .collect(),
                        },
                    }),
                    is_preferred_support: Some(true),
                    disabled_support: Some(true),
                    data_support: Some(true),
                    resolve_support: Some(CodeActionCapabilityResolveSupport {
                        properties: ["edit", "command"].into_iter().map(|s| s.into()).collect(),
                    }),
                    honors_change_annotations: None,
                }),
                code_lens: None,
                document_link: None,
                color_provider: None,
                rename: Some(RenameClientCapabilities {
                    dynamic_registration: Some(false),
                    prepare_support: Some(true),
                    prepare_support_default_behavior: None,
                    honors_change_annotations: Some(false),
                }),
                publish_diagnostics: Some(PublishDiagnosticsClientCapabilities {
                    related_information: None,
                    tag_support: None,
                    version_support: Some(true),
                    code_description_support: None,
                    data_support: None,
                }),
                folding_range: None,
                selection_range: None,
                linked_editing_range: None,
                call_hierarchy: None,
                semantic_tokens: None,
                moniker: None,
                type_hierarchy: None,
                inline_value: None,
                inlay_hint: Some(InlayHintClientCapabilities {
                    dynamic_registration: Some(false),
                    resolve_support: None,
                }),
                diagnostic: None,
            }),
            window: Some(WindowClientCapabilities {
                work_done_progress: Some(true),
                show_message: None,
                show_document: None,
            }),
            general: Some(GeneralClientCapabilities {
                regular_expressions: None,
                markdown: None,
                stale_request_support: None,
                position_encodings: Some(vec![
                    PositionEncodingKind::UTF8,
                    PositionEncodingKind::UTF16,
                    PositionEncodingKind::UTF32,
                ]),
            }),
            experimental: None,
        }
    }

    #[test]
    fn basic() -> anyhow::Result<()> {
        let (conn, handle) = start_server(helix_caps());

        conn.sender.send(Message::Request(lsp_server::Request {
            method: "shutdown".into(),
            params: serde_json::Value::Null,
        }))?;
        handle.join().unwrap()
    }
}
