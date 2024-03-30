// TODO: collapse adjacent text nodes

import gleam/io
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/string
import gleam/option.{type Option, None, Some}

pub type Document {
  Document(content: List(Container), references: Dict(String, String))
}

fn add_attribute(
  attributes: Dict(String, String),
  key: String,
  value: String,
) -> Dict(String, String) {
  case key {
    "class" ->
      dict.update(attributes, key, fn(previous) {
        case previous {
          None -> value
          Some(previous) -> previous <> " " <> value
        }
      })
    _ -> dict.insert(attributes, key, value)
  }
}

pub type Container {
  Paragraph(attributes: Dict(String, String), List(Inline))
  Heading(attributes: Dict(String, String), level: Int, content: List(Inline))
  Codeblock(
    attributes: Dict(String, String),
    language: Option(String),
    content: String,
  )
}

pub type Inline {
  Text(String)
  Link(content: List(Inline), destination: Destination)
  Strong(List(Inline))
  Emphasis(List(Inline))
}

pub type Destination {
  Reference(String)
  Url(String)
}

type Chars =
  List(String)

type Refs =
  Dict(String, String)

// TODO: document
pub fn to_html(djot: String) -> String {
  djot
  |> parse
  |> document_to_html
}

// TODO: document
pub fn parse(djot: String) -> Document {
  djot
  |> string.replace("\r\n", "\n")
  |> string.to_graphemes
  |> parse_document(dict.new(), [], dict.new())
}

fn drop_lines(in: Chars) -> Chars {
  case in {
    [] -> []
    ["\n", ..rest] -> drop_lines(rest)
    [c, ..rest] -> [c, ..rest]
  }
}

fn drop_spaces(in: Chars) -> Chars {
  case in {
    [] -> []
    [" ", ..rest] -> drop_spaces(rest)
    [c, ..rest] -> [c, ..rest]
  }
}

fn parse_document(
  in: Chars,
  refs: Refs,
  ast: List(Container),
  attrs: Dict(String, String),
) -> Document {
  let in = drop_lines(in)
  let in = drop_spaces(in)
  case in {
    [] -> Document(list.reverse(ast), refs)

    ["{", ..in2] ->
      case parse_attributes(in2, attrs) {
        None -> {
          let #(paragraph, in) = parse_paragraph(in, attrs)
          parse_document(in, refs, [paragraph, ..ast], dict.new())
        }
        Some(#(attrs, in)) -> parse_document(in, refs, ast, attrs)
      }

    ["#", ..in] -> {
      let #(heading, refs, in) = parse_heading(in, refs, attrs)
      parse_document(in, refs, [heading, ..ast], dict.new())
    }

    ["~" as delim, ..in2] | ["`" as delim, ..in2] -> {
      case parse_codeblock(in2, attrs, delim) {
        None -> {
          let #(paragraph, in) = parse_paragraph(in, attrs)
          parse_document(in, refs, [paragraph, ..ast], dict.new())
        }
        Some(#(codeblock, in)) ->
          parse_document(in, refs, [codeblock, ..ast], dict.new())
      }
    }

    ["[", ..in2] -> {
      case parse_ref_def(in2, "") {
        None -> {
          let #(paragraph, in) = parse_paragraph(in, attrs)
          parse_document(in, refs, [paragraph, ..ast], dict.new())
        }
        Some(#(id, url, in)) -> {
          let refs = dict.insert(refs, id, url)
          parse_document(in, refs, ast, dict.new())
        }
      }
    }

    _ -> {
      let #(paragraph, in) = parse_paragraph(in, attrs)
      parse_document(in, refs, [paragraph, ..ast], dict.new())
    }
  }
}

fn parse_codeblock(
  in: Chars,
  attrs: Dict(String, String),
  delim: String,
) -> Option(#(Container, Chars)) {
  use #(language, count, in) <- option.then(parse_codeblock_start(in, delim, 1))
  let #(content, in) = parse_codeblock_content(in, delim, count, "")
  Some(#(Codeblock(attrs, language, content), in))
}

fn parse_codeblock_start(
  in: Chars,
  delim: String,
  count: Int,
) -> Option(#(Option(String), Int, Chars)) {
  case in {
    [c, ..in] if c == delim -> parse_codeblock_start(in, delim, count + 1)
    ["\n", ..in] if count >= 3 -> Some(#(None, count, in))
    [_, ..] if count >= 3 -> {
      let in = drop_spaces(in)
      let #(language, in) = parse_codeblock_language(in, "")
      Some(#(language, count, in))
    }
    _ -> None
  }
}

fn parse_codeblock_content(
  in: Chars,
  delim: String,
  count: Int,
  acc: String,
) -> #(String, Chars) {
  case parse_codeblock_end(in, delim, count) {
    None -> {
      let #(acc, in) = slurp_verbatim_line(in, acc)
      parse_codeblock_content(in, delim, count, acc)
    }
    Some(#(in)) -> #(acc, in)
  }
}

fn slurp_verbatim_line(in: Chars, acc: String) -> #(String, Chars) {
  case in {
    [] -> #(acc, [])
    ["\n", ..in] -> #(acc <> "\n", in)
    [c, ..in] -> slurp_verbatim_line(in, acc <> c)
  }
}

fn parse_codeblock_end(in: Chars, delim: String, count: Int) -> Option(#(Chars)) {
  case in {
    ["\n", ..in] if count == 0 -> Some(#(in))
    _ if count == 0 -> Some(#(in))

    [c, ..in] if c == delim -> parse_codeblock_end(in, delim, count - 1)

    [] -> Some(#(in))
    _ -> None
  }
}

fn parse_codeblock_language(
  in: Chars,
  language: String,
) -> #(Option(String), Chars) {
  case in {
    [] -> #(None, in)
    ["\n", ..in] if language == "" -> #(None, in)
    ["\n", ..in] -> #(Some(language), in)
    [c, ..in] -> parse_codeblock_language(in, language <> c)
  }
}

fn parse_ref_def(in: Chars, id: String) -> Option(#(String, String, Chars)) {
  case in {
    ["]", ":", ..in] -> parse_ref_value(in, id, "")
    [] | ["]", ..] -> None
    [c, ..in] -> parse_ref_def(in, id <> c)
  }
}

fn parse_ref_value(
  in: Chars,
  id: String,
  url: String,
) -> Option(#(String, String, Chars)) {
  case in {
    [] -> Some(#(id, string.trim(url), []))
    ["\n", ..in] -> Some(#(id, string.trim(url), in))
    [c, ..in] -> parse_ref_value(in, id, url <> c)
  }
}

fn parse_attributes(
  in: Chars,
  attrs: Dict(String, String),
) -> Option(#(Dict(String, String), Chars)) {
  let in = drop_spaces(in)
  case in {
    [] -> None
    ["}", ..in] -> parse_attributes_end(in, attrs)
    ["#", ..in] -> {
      case parse_attributes_id_or_class(in, "") {
        Some(#(id, in)) -> parse_attributes(in, add_attribute(attrs, "id", id))
        None -> None
      }
    }
    [".", ..in] -> {
      case parse_attributes_id_or_class(in, "") {
        Some(#(c, in)) -> parse_attributes(in, add_attribute(attrs, "class", c))
        None -> None
      }
    }
    _ -> {
      case parse_attribute(in, "") {
        Some(#(k, v, in)) -> parse_attributes(in, add_attribute(attrs, k, v))
        None -> None
      }
    }
  }
}

fn parse_attribute(in: Chars, key: String) -> Option(#(String, String, Chars)) {
  case in {
    [] | [" ", ..] -> None
    ["=", "\"", ..in] -> parse_attribute_quoted_value(in, key, "")
    ["=", ..in] -> parse_attribute_value(in, key, "")
    [c, ..in] -> parse_attribute(in, key <> c)
  }
}

fn parse_attribute_value(
  in: Chars,
  key: String,
  value: String,
) -> Option(#(String, String, Chars)) {
  case in {
    [] -> None
    [" ", ..in] -> Some(#(key, value, in))
    ["}", ..] -> Some(#(key, value, in))
    [c, ..in] -> parse_attribute_value(in, key, value <> c)
  }
}

fn parse_attribute_quoted_value(
  in: Chars,
  key: String,
  value: String,
) -> Option(#(String, String, Chars)) {
  case in {
    [] -> None
    ["\"", ..in] -> Some(#(key, value, in))
    [c, ..in] -> parse_attribute_quoted_value(in, key, value <> c)
  }
}

fn parse_attributes_id_or_class(
  in: Chars,
  id: String,
) -> Option(#(String, Chars)) {
  case in {
    [] | ["}", ..] | [" ", ..] -> Some(#(id, in))
    ["#", ..] | [".", ..] | ["=", ..] -> None
    // TODO: in future this will be permitted as attributes can be over multiple lines
    ["\n", ..] -> None
    [c, ..in] -> parse_attributes_id_or_class(in, id <> c)
  }
}

fn parse_attributes_end(
  in: Chars,
  attrs: Dict(String, String),
) -> Option(#(Dict(String, String), Chars)) {
  case in {
    [] -> Some(#(attrs, []))
    ["\n", ..in] -> Some(#(attrs, in))
    [" ", ..in] -> parse_attributes_end(in, attrs)
    [_, ..] -> None
  }
}

fn parse_heading(
  in: Chars,
  refs: Refs,
  attrs: Dict(String, String),
) -> #(Container, Refs, Chars) {
  case heading_level(in, 1) {
    Some(#(level, in)) -> {
      let in = drop_spaces(in)
      let #(inline_in, in) = take_heading_chars(in, level, [])
      let inline = parse_inline(inline_in, "", [])
      let text = take_inline_text(inline, "")
      let #(refs, attrs) = case id_sanitise(text) {
        "" -> #(refs, attrs)
        id -> {
          case dict.get(refs, id) {
            Ok(_) -> #(refs, attrs)
            Error(_) -> {
              let refs = dict.insert(refs, id, "#" <> id)
              let attrs = add_attribute(attrs, "id", id)
              #(refs, attrs)
            }
          }
        }
      }
      let heading = Heading(attrs, level, inline)
      #(heading, refs, in)
    }

    None -> {
      let #(p, in) = parse_paragraph(["#", ..in], attrs)
      #(p, refs, in)
    }
  }
}

fn id_sanitise(content: String) -> String {
  content
  |> string.to_graphemes
  |> list.filter(id_char)
  |> id_escape("")
}

fn id_char(char: String) -> Bool {
  case char {
    "#" | "?" | "!" | "," -> False
    _ -> True
  }
}

fn id_escape(content: Chars, acc: String) -> String {
  case content {
    [] -> acc

    [" ", ..rest] | ["\n", ..rest] if rest == [] -> acc
    [" ", ..rest] | ["\n", ..rest] if acc == "" -> id_escape(rest, acc)

    [" ", ..rest] | ["\n", ..rest] -> id_escape(rest, acc <> "-")

    [c, ..rest] -> id_escape(rest, acc <> c)
  }
}

fn take_heading_chars(in: Chars, level: Int, acc: Chars) -> #(Chars, Chars) {
  case in {
    [] | ["\n"] -> #(list.reverse(acc), [])
    ["\n", "\n", ..in] -> #(list.reverse(acc), in)
    ["\n", "#", ..rest] -> {
      case take_heading_chars_newline_hash(rest, level - 1, ["\n", ..acc]) {
        Some(#(acc, in)) -> take_heading_chars(in, level, acc)
        None -> #(list.reverse(acc), in)
      }
    }
    [c, ..in] -> take_heading_chars(in, level, [c, ..acc])
  }
}

fn take_heading_chars_newline_hash(
  in: Chars,
  level: Int,
  acc: Chars,
) -> Option(#(Chars, Chars)) {
  case in {
    _ if level < 0 -> None
    [] if level > 0 -> None

    [] if level == 0 -> Some(#(acc, []))
    [" ", ..in] if level == 0 -> Some(#(acc, in))

    ["#", ..rest] -> take_heading_chars_newline_hash(rest, level - 1, acc)

    _ -> None
  }
}

type InlineParser =
  fn(Chars, String) -> Option(#(List(Inline), Chars))

// takes a list of parsers and returns the first successfully parsed result
fn parse_any(
  in: Chars,
  text: String,
  acc: List(Inline),
  of parsers: List(InlineParser),
) -> Option(#(Inline, Chars)) {
  list.fold_until(parsers, None, fn(_, f) {
    case f(in, text, acc) {
      None -> list.Continue(None)
      parsed -> list.Stop(parsed)
    }
  })
}

fn parse_inline(in: Chars, text: String, acc: List(Inline)) -> List(Inline) {
  case in {
    [] if text == "" -> list.reverse(acc)
    [] -> parse_inline([], "", [Text(text), ..acc])
    [c, ..] ->
      case
        parse_any(in, text, acc, of: [
          link,
          emphasis("*"),
          emphasis("_"),
          smart_punctuation,
        ])
      {
        // Some(#(inline, in)) -> parse_inline(in, "", [inline, Text(text), ..acc])
        Some(#(parsed_acc, in)) -> parse_inline(in, "", parsed_acc)
        None -> parse_inline(rest, text <> c, acc)
      }
  }
}

fn smart_punctuation(in: Chars) {
  case in {
    ["{", c, ..rest] -> {
      case c {
        "'" -> {
          case take_until_forced_closing("'", rest, []) {
            Some(#(chars, rest)) -> {
              let inline = parse_inline(chars, "", [])
              Some(#(SmartSingle(inline), rest))
            }
            None -> None
          }
        }
        "\"" -> {
          case take_until_forced_closing("\"", rest, []) {
            Some(#(chars, rest)) -> {
              let inline = parse_inline(chars, "", [])
              Some(#(SmartDouble(inline), rest))
            }
            None -> None
          }
        }
      }
    }
    ["-", "-", "-", ..rest] -> Some(#(SmartHyphen(3), rest))
    [".", ".", ".", ..rest] -> Some(#(SmartEllipsis, rest))
    ["'", ..rest] -> {
      case take_until_closing("'", rest, []) {
        Some(#(chars, rest)) -> {
          let inline = parse_inline(chars, "", [])
          Some(#(SmartSingle(inline), rest))
        }
        None -> None
      }
    }
    ["\"", ..rest] -> {
      case take_until_closing("\"", rest, []) {
        Some(#(chars, rest)) -> {
          let inline = parse_inline(chars, "", [])
          Some(#(SmartDouble(inline), rest))
        }
        None -> None
      }
    }
    [c, ..rest] -> None
  }
}

fn take_until_forced_closing(
  delim: String,
  in: Chars,
  acc: Chars,
) -> Option(#(Chars, Chars)) {
  case in {
    [] -> None
    [c, "}", ..rest] if c == delim -> Some(#(acc, rest))
    [c, ..rest] -> take_until_forced_closing(delim, rest, [c, ..acc])
  }
}

fn take_until_closing(
  delim: String,
  in: Chars,
  acc: Chars,
) -> Option(#(Chars, Chars)) {
  case in {
    [] -> None
    [c, ..rest] if c == delim -> Some(#(acc, rest))
    [c, ..rest] -> take_until_closing(delim, rest, [c, ..acc])
  }
}

fn take_until_last(
  delim: String,
  in: Chars,
  acc: Chars,
  result: Option(#(Chars, Chars)),
) -> Option(#(Chars, Chars)) {
  case in {
    [] -> result
    [c, ..rest] if c == delim ->
      take_until_last(delim, rest, [c, ..acc], Some(#(acc, rest)))
    [c, ..rest] -> take_until_last(delim, rest, [c, ..acc], result)
  }
}

fn link(in: Chars) {
  case take_link_chars(in, []) {
    // This wasn't a link, it was just a `[` in the text
    None -> None

    Some(#(inline_in, ref, in)) -> {
      let inline = parse_inline(inline_in, "", [])
      let link = Link(inline, ref)
      Some(#(link, in))
    }
  }
}

fn take_link_chars(
  in: Chars,
  inline_in: Chars,
) -> Option(#(Chars, Destination, Chars)) {
  case in {
    // This wasn't a link, it was just a `[..]` in the text
    [] -> None

    ["]", "[", ..in] -> {
      let inline_in = list.reverse(inline_in)
      take_link_chars_destination(in, False, inline_in, "")
    }
    ["]", "(", ..in] -> {
      let inline_in = list.reverse(inline_in)
      take_link_chars_destination(in, True, inline_in, "")
    }
    [c, ..rest] -> take_link_chars(rest, [c, ..inline_in])
  }
}

fn take_link_chars_destination(
  in: Chars,
  is_url: Bool,
  inline_in: Chars,
  acc: String,
) -> Option(#(Chars, Destination, Chars)) {
  case in {
    [] -> None

    [")", ..in] if is_url -> Some(#(inline_in, Url(acc), in))
    ["]", ..in] if !is_url -> Some(#(inline_in, Reference(acc), in))

    ["\n", ..rest] -> take_link_chars_destination(rest, is_url, inline_in, acc)
    [c, ..rest] ->
      take_link_chars_destination(rest, is_url, inline_in, acc <> c)
  }
}

// parse strong or regular emphasis where em is either "*" or "_"
fn emphasis(em: String) -> InlineParser {
  fn(in: Chars, text, acc) {
    case in {
      [] -> None
      [c, ..rest] if c == em -> {
        // ensure the previous and next characters are non-whitespace
        let next_is_whitespace = case rest {
          [] -> False
          [c, ..] -> is_whitespace(c)
        }

        case string.last(text), next_is_whitespace {
          _, True -> None
          Ok(c), False ->
            case is_whitespace(c) {
              True -> None
              False -> take_emphasis_chars(em, rest, [])
            }
          _, False -> take_emphasis_chars(em, rest, [])
        }
      }
      [_, ..] -> None
    }
  }
}

fn take_emphasis_chars(em: String, in: Chars, acc: List(String)) {
  case in {
    [] -> None
    [e, ..rest] if e == em -> {
      // check if the previous character was whitespace
      case acc {
        [] -> take_emphasis_chars(em, rest, [e, ..acc])
        [c, ..] -> {
          case is_whitespace(c) {
            True -> take_emphasis_chars(em, rest, [e, ..acc])
            False -> {
              let acc = list.reverse(acc)

              let emphasis =
                parse_inline(acc, "", [])
                |> container_for_emphasis(e)

              Some(#(emphasis, rest))
            }
          }
        }
      }
    }
    [c, ..rest] -> take_emphasis_chars(em, rest, [c, ..acc])
  }
}

fn is_whitespace(c: String) -> Bool {
  c == " " || c == "\n" || c == "\t"
}

fn container_for_emphasis(c: String) {
  case c {
    "*" -> Strong
    "_" -> Emphasis
    _ -> {
      io.print_error("Unknown emphasis character: " <> c)
      panic
    }
  }
}

fn heading_level(in: Chars, level: Int) -> Option(#(Int, Chars)) {
  case in {
    ["#", ..rest] -> heading_level(rest, level + 1)

    [] if level > 0 -> Some(#(level, []))
    [" ", ..rest] | ["\n", ..rest] if level != 0 -> Some(#(level, rest))

    _ -> None
  }
}

fn take_inline_text(inlines: List(Inline), acc: String) -> String {
  case inlines {
    [] -> acc
    [first, ..rest] ->
      case first {
        Text(text) -> take_inline_text(rest, acc <> text)
        Link(nested, _) -> {
          let acc = take_inline_text(nested, acc)
          take_inline_text(rest, acc)
        }
        Strong(nested) -> {
          let acc = take_inline_text(nested, acc)
          take_inline_text(rest, acc)
        }
        Emphasis(nested) -> {
          let acc = take_inline_text(nested, acc)
          take_inline_text(rest, acc)
        }
        SmartSingle(nested) -> {
          let acc = take_inline_text(nested, acc)
          take_inline_text(rest, acc)
        }
        SmartDouble(nested) -> {
          let acc = take_inline_text(nested, acc)
          take_inline_text(rest, acc)
        }
        SmartHyphen(_) -> take_inline_text(rest, acc)
        SmartEllipsis -> take_inline_text(rest, acc)
      }
  }
}

fn parse_paragraph(
  in: Chars,
  attrs: Dict(String, String),
) -> #(Container, Chars) {
  let #(inline_in, in) = take_paragraph_chars(in, [])
  let inline = parse_inline(inline_in, "", [])
  #(Paragraph(attrs, inline), in)
}

fn take_paragraph_chars(in: Chars, acc: Chars) -> #(Chars, Chars) {
  case in {
    [] | ["\n"] -> #(list.reverse(acc), [])
    ["\n", "\n", ..rest] -> #(list.reverse(acc), rest)
    [c, ..rest] -> take_paragraph_chars(rest, [c, ..acc])
  }
}

// TODO: document
pub fn document_to_html(document: Document) -> String {
  containers_to_html(document.content, document.references, "")
}

fn containers_to_html(
  containers: List(Container),
  refs: Refs,
  html: String,
) -> String {
  case containers {
    [] -> html
    [container, ..rest] -> {
      let html = container_to_html(html, container, refs)
      containers_to_html(rest, refs, html)
    }
  }
}

fn container_to_html(html: String, container: Container, refs: Refs) -> String {
  case container {
    Paragraph(attrs, inlines) -> {
      html
      |> open_tag("p", attrs)
      |> inlines_to_html(inlines, refs)
      |> close_tag("p")
    }

    Codeblock(attrs, language, content) -> {
      let code_attrs = case language {
        Some(lang) -> add_attribute(attrs, "class", "language-" <> lang)
        None -> attrs
      }
      html
      |> open_tag("pre", dict.new())
      |> open_tag("code", code_attrs)
      |> string.append(content)
      |> close_tag("code")
      |> close_tag("pre")
    }

    Heading(attrs, level, inlines) -> {
      let tag = "h" <> int.to_string(level)
      html
      |> open_tag(tag, attrs)
      |> inlines_to_html(inlines, refs)
      |> close_tag(tag)
    }
  }
  <> "\n"
}

fn open_tag(
  html: String,
  tag: String,
  attributes: Dict(String, String),
) -> String {
  let html = html <> "<" <> tag
  attributes_to_html(html, attributes) <> ">"
}

fn close_tag(html: String, tag: String) -> String {
  html <> "</" <> tag <> ">"
}

fn inlines_to_html(html: String, inlines: List(Inline), refs: Refs) -> String {
  case inlines {
    [] -> html
    [inline, ..rest] -> {
      html
      |> inline_to_html(inline, refs)
      |> inlines_to_html(rest, refs)
      |> string.trim_right
    }
  }
}

fn inline_to_html(html: String, inline: Inline, refs: Refs) -> String {
  case inline {
    Text(text) -> html <> text
    Link(text, destination) -> {
      html
      |> open_tag("a", destination_attribute(destination, refs))
      |> inlines_to_html(text, refs)
      |> close_tag("a")
    }
    Strong(inlines) -> {
      html
      |> open_tag("strong", dict.new())
      |> inlines_to_html(inlines, refs)
      |> close_tag("strong")
    }
    Emphasis(inlines) -> {
      html
      |> open_tag("em", dict.new())
      |> inlines_to_html(inlines, refs)
      |> close_tag("em")
    }
  }
}

fn destination_attribute(
  destination: Destination,
  refs: Refs,
) -> Dict(String, String) {
  let dict = dict.new()
  case destination {
    Url(url) -> dict.insert(dict, "href", url)
    Reference(id) ->
      case dict.get(refs, id) {
        Ok(url) -> dict.insert(dict, "href", url)
        Error(Nil) -> dict
      }
  }
}

fn attributes_to_html(html: String, attributes: Dict(String, String)) -> String {
  attributes
  |> dict.to_list
  |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
  |> list.fold(html, fn(html, pair) {
    html <> " " <> pair.0 <> "=\"" <> pair.1 <> "\""
  })
}
