
open Parser.Parsers;

let parser = orElse(string("abra"), string("cadabra"))

switch(run(parser, "cadabra")) {
| Ok((result, loc)) =>
    Js.log({
        "success": true,
        "value": result,
        "line": loc->Location.line,
        "column": loc -> Location.col
    })
| Err(e) =>
    Js.log({
        "success": false,
        "error": Parser.ParseError.toString(e),
    })
}
