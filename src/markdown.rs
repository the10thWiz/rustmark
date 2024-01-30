use std::ops::{RangeFrom, RangeTo};

use anyhow::anyhow;
use nom::{
    branch::alt,
    bytes::complete::{
        tag, take, take_till, take_till1, take_until, take_until1, take_while, take_while1,
    },
    character::{
        complete::{anychar, line_ending, one_of},
        streaming::{anychar, line_ending},
    },
    combinator::{all_consuming, eof, map, not, recognize},
    error::{ErrorKind, ParseError},
    multi::{many0, many0_count, many1, many1_count, many_till, separated_list0},
    sequence::tuple,
    IResult, InputIter, InputLength, Offset, Parser, Slice,
};
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

pub struct MarkdownDocument<'a> {
    parts: Vec<Part<'a>>,
}

pub enum Part<'a> {
    Header {
        level: usize,
        value: Span<'a>,
        parts: Vec<TextPart<'a>>,
    },
    Paragraph(Span<'a>),
    List {
        parts: Vec<ListPart<'a>>,
        ordered: bool,
    },
    BlockQuote {
        // TODO
    },
    CodeBlock {
        indent: Indent,
        raw: Span<'a>,
    },
    HorizontalRule(Span<'a>),
}

enum Indent {
    Spaces(usize),
    Tabs(usize),
}

pub struct ListPart<'a> {
    level: usize,
    label: Span<'a>,
    parts: Vec<TextPart<'a>>,
}

pub enum TextPart<'a> {
    Plain(Span<'a>),
    Code(Surrounded<'a>),
    Bold(Surrounded<'a>),
    Italics(Surrounded<'a>),
    BoldAndItalics(Surrounded<'a>),
    StrikeThrough(Surrounded<'a>),
    Link {
        text: Surrounded<'a, Vec<Span<'a>>>,
        url: Surrounded<'a>,
        title: Option<Surrounded<'a>>,
    },
    Url(Surrounded<'a>),
    Image {
        // TODO
        tag: Span<'a>,
        text: Surrounded<'a, Vec<Span<'a>>>,
        url: Surrounded<'a>,
        title: Option<Surrounded<'a>>,
    },
}

pub struct Surrounded<'a, V = Span<'a>> {
    start: Span<'a>,
    value: V,
    end: Span<'a>,
}

pub fn parse_markdown<'a>(doc: &'a str) -> anyhow::Result<MarkdownDocument<'a>> {
    let span = Span::<'a>::new(doc);
    // Map_err converts the error to an owned error
    let (_, parts) = all_consuming(separated_list0(many1(empty_line), any_part))(span)
        .map_err(|e| anyhow!("Error: {}", e))?;
    // nom::multi::separated_list0(, )
    Ok(MarkdownDocument { parts })
}

fn any_part<'a>(i: Span<'a>) -> IResult<Span<'a>, Part<'a>> {
    // Skip empty lines
    let (i, _) = many0_count(empty_line)(i)?;
    alt((header, horizontal_rule))(i)
}

fn header<'a>(i: Span<'a>) -> IResult<Span<'a>, Part<'a>> {
    let (i, level) = many1_count(tag("#"))(i)?;
    let (i, _) = ws(i)?;
    let (i, line) = recognize(many_till_count(anychar, line_ending))(i)?;
    let (_, parts) = text_parts(line)?;
    Ok((
        i,
        Part::Header {
            level,
            value: line,
            parts,
        },
    ))
}

fn text_parts<'a>(i: Span<'a>) -> IResult<Span<'a>, Vec<TextPart<'a>>> {
    fn bold<'a>(i: Span<'a>) -> IResult<Span<'a>, TextPart<'a>> {
        let (i, start) = alt((tag("**"), tag("__")))(i)?;
        let (i, value) = take_anychar_till(tag(*start.fragment()))(i)?;
        let (i, end) = tag(*start.fragment())(i)?;
        Ok((i, TextPart::Bold(Surrounded { start, value, end })))
    }
    fn italics<'a>(i: Span<'a>) -> IResult<Span<'a>, TextPart<'a>> {
        let (i, start) = alt((tag("*"), tag("_")))(i)?;
        let (i, value) = take_anychar_till(tag(*start.fragment()))(i)?;
        let (i, end) = tag(*start.fragment())(i)?;
        Ok((i, TextPart::Italics(Surrounded { start, value, end })))
    }
    fn bold_and_italics<'a>(i: Span<'a>) -> IResult<Span<'a>, TextPart<'a>> {
        let (i, start) = alt((tag("***"), tag("___")))(i)?;
        let (i, value) = take_anychar_till(tag(*start.fragment()))(i)?;
        let (i, end) = tag(*start.fragment())(i)?;
        Ok((
            i,
            TextPart::BoldAndItalics(Surrounded { start, value, end }),
        ))
    }
    fn code<'a>(i: Span<'a>) -> IResult<Span<'a>, TextPart<'a>> {
        let (i, start) = tag("`")(i)?;
        let (i, value) = take_anychar_till(tag("`"))(i)?;
        let (i, end) = tag("`")(i)?;
        Ok((i, TextPart::Code(Surrounded { start, value, end })))
    }

    fn text_part<'a>(i: Span<'a>) -> IResult<Span<'a>, TextPart<'a>> {
        alt((bold_and_italics, bold, italics, code))(i)
    }
    fn plain_or_part<'a>(i: Span<'a>) -> IResult<Span<'a>, TextPart<'a>> {
        alt((
            text_part,
            map(take_anychar_till(text_part), |s| TextPart::Plain(s)),
        ))(i)
    }
    map(many_till(plain_or_part, eof), |(v, _)| v)(i)
}

fn horizontal_rule<'a>(i: Span<'a>) -> IResult<Span<'a>, Part<'a>> {
    fn ident<'a>(i: Span<'a>) -> IResult<Span<'a>, ()> {
        let (i, ch) = one_of("*_-")(i)?;
        match ch {
            '*' => {
                let (i, _) = tag("**")(i)?;
                let (i, _) = many0(tag("*"))(i)?;
                Ok((i, ()))
            }
            '-' => {
                let (i, _) = tag("--")(i)?;
                let (i, _) = many0(tag("-"))(i)?;
                Ok((i, ()))
            }
            '_' => {
                let (i, _) = tag("__")(i)?;
                let (i, _) = many0(tag("_"))(i)?;
                Ok((i, ()))
            }
            _ => unreachable!(),
        }
    }
    let (i, _) = ws(i)?;
    let (i, value) = nom::combinator::recognize(ident)(i)?;
    let (i, _) = ws(i)?;
    let (i, _) = line_ending(i)?;
    Ok((i, Part::HorizontalRule(value)))
}

/// Consume an empty line
fn empty_line(i: Span) -> IResult<Span, Span> {
    let (i, _) = ws(i)?;
    line_ending(i)
}

/// Consume all whitespace
fn ws(i: Span) -> IResult<Span, Span> {
    take_while(|b: char| b.is_whitespace())(i)
}

/// Consumes any characters until f produces a result, but skips any character preceeded by a `\`
fn take_anychar_till<I, O, E>(f: impl Parser<I, O, E>) -> impl FnMut(I) -> IResult<I, I, E>
where
    I: Clone
        + InputLength
        + InputIter<Item = char>
        + Offset
        + Slice<RangeTo<usize>>
        + Slice<RangeFrom<usize>>,
    E: ParseError<I>,
{
    fn single_char<I, E>(i: I) -> IResult<I, (), E>
    where
        I: Clone + InputLength + InputIter<Item = char> + Slice<RangeFrom<usize>>,
        E: ParseError<I>,
    {
        let (i, c) = anychar(i)?;
        if c == '\\' {
            let (i, _) = anychar(i)?;
            Ok((i, ()))
        } else {
            Ok((i, ()))
        }
    }
    recognize(many_till_count(single_char, f))
}

/// Applies the parser `f` until the parser `g` produces a result.
///
/// Returns a tuple of the number of results of `f` and the result of `g`.
///
/// `f` keeps going so long as `g` produces [`Err::Error`]. To instead chain an error up, see [`cut`][crate::combinator::cut].
fn many_till_count<I, O, P, E, F, G>(
    mut f: F,
    mut g: G,
) -> impl FnMut(I) -> IResult<I, (usize, P), E>
where
    I: Clone + InputLength,
    F: Parser<I, O, E>,
    G: Parser<I, P, E>,
    E: ParseError<I>,
{
    use nom::Err;
    move |mut i| {
        let mut res = 0;
        loop {
            let len = i.input_len();
            match g.parse(i.clone()) {
                Ok((i1, o)) => return Ok((i1, (res, o))),
                Err(Err::Error(_)) => {
                    match f.parse(i.clone()) {
                        Err(Err::Error(err)) => {
                            return Err(Err::Error(E::append(i, ErrorKind::ManyTill, err)))
                        }
                        Err(e) => return Err(e),
                        Ok((i1, _o)) => {
                            // infinite loop check: the parser must always consume
                            if i1.input_len() == len {
                                return Err(Err::Error(E::from_error_kind(
                                    i1,
                                    ErrorKind::ManyTill,
                                )));
                            }

                            res += 1;
                            i = i1;
                        }
                    }
                }
                Err(e) => return Err(e),
            }
        }
    }
}
