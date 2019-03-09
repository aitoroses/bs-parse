open Util;

module type Parsers = {

  type parser('a)
  type parse_error
  /*let run: parser('a) => string => result('a, parse_error)*/
  let string: string => parser(string)
  let orElse: (parser('a), parser('a)) => parser('a)
  let flatMap: parser('a) => ('a => parser('b)) => parser('b)
  let unit: 'a => parser('a)
  let listOfN: int => parser('a) => parser(list('a))
  let many: parser('a) => parser(list('a))
  let many1: parser('a) => parser(list('a))
  let slice: parser('a) => parser(string)
  let regex: Js.Re.t => parser(string)

  /* seq(p)(f): Run a parser, then use its result to select a second parser to run in sequence 
   * for me this one is flatMap
   */
  
  let label: string => parser('a) => parser('a)
  let scope: string => parser('a) => parser('a)

  let attempt: parser('a) => parser('a)

};

module DerivedParsers = (PS: Parsers) => {
  open PS;

  let map = parser => fn => flatMap(parser, v => unit(fn(v)))

  let map2 = (parser1, parser2) => fn =>
    flatMap(parser1, v1 => 
      map(parser2, v2 => fn(v1, v2)))

  let product = (parser1, parser2) => map2(parser1, parser2, (v1, v2) => (v1, v2))

  let char = c => char_to_string(c) |> string |. map(s => s.[0])
};

module InfixOps = (PS: Parsers) => {
  module DP = DerivedParsers(PS);
  open PS;
  open DP;

  let (>>=) = flatMap
  let (<$>) = map
  let (>>) = product
  let (<|>) = orElse
}

module ParseError = {
  type t = {
    stack: list((Location.t, string)),
    otherFailures: list(t)
  }
  let makeWith = (stack, failures) => { stack: stack, otherFailures: failures }
  let make = loc => string => makeWith([(loc, string)], [])

  let getStackTrace = error => List.fold_left((acc, (loc, message)) => {
      let line = Location.line(loc)
      let column = Location.col(loc)
      let finalMessage = {j|$message at line $line, column $column|j}
      let res = Js.Array.concat([|finalMessage|], acc)
      res
    }, [||], error.stack)

  let getAllStackTrace = error => {
    let otherFailureStackTraces = 
      error.otherFailures 
      |> Array.of_list 
      |> Array.map(getStackTrace) 
      |> Array.fold_left(Array.append, [||])
    Array.append(getStackTrace(error), otherFailureStackTraces)
  }

  let toString = error => error |> getAllStackTrace |> Js.Array.joinWith("\n")
}

module type ErrorReporting = {
  type parser('a)
  type parse_error
  let errorLocation: parse_error => Location.t
  let errorMessage: parse_error => string
  let errorStack: parse_error => list((Location.t,string))
}


module Parsers = {

  type parse_error = ParseError.t
  type parser('a) = Parser(Location.t => result(('a, Location.t), parse_error))

  let runParser = p => loc => switch(p) {
  | Parser(fn) => fn(loc)
  }

  let run = parser => input => {
    runParser(parser, Location.make(input, 0))
  }

  exception CannotGet(ParseError.t)
  exception CannotGetError

  let get_exn = result => switch(result) {
  | Ok((res, _)) => res
  | Err(error) => raise(CannotGet(error))
  }

  let get_error = result => switch(result) {
  | Err(error) => error
  | _ => raise(CannotGetError)
  }

  let string = str => Parser(loc => {
    open Js.String;
    let substr = loc.input->substr(~from=loc.offset)
    if (substr |> startsWith(str)) {
      let charsConsumed = str->length
      Ok((str, loc->Location.inc(charsConsumed)))
    } else {
      Err(ParseError.make(loc, "Expected: " ++ str))
    }
  })

  let orElse = p1 => p2 => Parser(loc => {
    switch(runParser(p1, loc)) {
    | Err(error1) =>
      switch(runParser(p2,loc)) {
      | Err(error2) =>
        let stack = error2.stack
        let otherFailures = [error1, ...error2.otherFailures]
        Err(ParseError.makeWith(stack, otherFailures))
      | ok => ok
      }
    | ok => ok
    }
  })

  let flatMap = p => fn => Parser(loc => {
    switch(runParser(p, loc)) {
    | Ok((v1, loc)) =>
      switch(runParser(fn(v1), loc)) {
      | Ok((v2, loc)) => Ok((v2, loc))
      | err => err
      }
    | err => err
    }
  })

  let unit = a => Parser(loc => Ok((a, loc)))

  let listOfN = int => p => Parser(loc => {
    let rec run = (i, p, loc, acc) => {
      switch(runParser(p, loc)) {
      | Ok((v, loc)) =>
        let newAcc = Array.append(acc, [|v|])
        if (i <= 0) {
          Ok((newAcc, loc))
        } else {
          run(i-1, p, loc, newAcc)
        }
      | Err(error) => Err(error)  
      }
    }
    run(int, p, loc, [||])
  })

  let many = p => Parser(loc => {
    let rec run = (p, loc, acc) => {
      switch(runParser(p, loc)) {
      | Ok((v, loc)) => run(p, loc, Array.append(acc, [|v|]))
      | _ => Ok((acc, loc))
      }
    }
    run(p, loc, [||])
  })

  let many1 = p => Parser(loc => {
    switch(runParser(many(p), loc)) {
    | Ok((v, loc)) =>
      if (Array.length(v) === 0) {
        Err(ParseError.make(loc, "Expected at least one repetition for parser"))
      } else {
        Ok((v, loc))
      }
    | err => err
    }
  })

  let slice = p => Parser(loc => {
    switch(runParser(p, loc)) {
    | Ok((_, newLoc)) => 
      let charsConsumed = newLoc.offset - loc.offset
      Ok((loc.input->Js.String.substrAtMost(~from=loc.offset, ~length=charsConsumed), newLoc))
    | Err(error) => Err(error)
    }
  })

  let regex = regexpr => Parser(loc => {
    let reg = Js.Re.fromString("^" ++ regexpr)
    if (Js.Re.test(loc.input, reg)) {
      let result = loc.input
        ->Js.Re.exec(reg)
        ->Belt.Option.getExn
        ->Js.Re.captures
        |> Array.map(x => Js.Nullable.toOption(x) |> Belt.Option.getExn)
      let charsConsumed = result->Array.get(0)->String.length
      // calculate the difference
      let newLoc = {
        ...loc,
        offset: loc.offset + charsConsumed
      }
      Ok((result, newLoc))
    } else {
      Err(ParseError.make(loc, "Expected: " ++ regexpr))
    }
  })
}

