

type result('a, 'b) =
  | Ok('a)
  | Err('b);

let explode = str => {
  let rec exp = i => list =>
    if (i < 0) {
      list
    } else {
      exp(i - 1, [str.[i], ...list])
    };
  exp(String.length(str) - 1, [])
};

let char_to_string = String.make(1)

let char_list_to_string = list => List.fold_left((++), "") @@ List.map(char_to_string) @@ list

let rec take = (n, list) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => n == 0 ? [] : [x, ...take(n - 1, xs)]
  };

module type Parser = {
  type parser('a)
  type parse_error
};

module type Parsers = (P: Parser) => {
  type parser('a) = P.parser('a);
  type parse_error = P.parse_error;
  let run: parser('a) => string => result('a, parse_error)
  let string: string => parser(string)
  let orElse: (parser('a), parser('a)) => parser('a)
  let flatMap: parser('a) => ('a => parser('b)) => parser('b)
  let unit: 'a => parser('a)
  let listOfN: int => parser('a) => parser(list('a))
  let many: parser('a) => parser(list('a))
  let many1: parser('a) => parser(list('a))
  let slice: parser('a) => parser(string)
  let regex: Js.Re.t => parser(string)
};

module DerivedParsers = (P: Parser, PS: Parsers) => {
  module Parsers = PS(P);
  open Parsers;

  let map = parser => fn => flatMap(parser, v => unit(fn(v)))

  let map2 = (parser1, parser2) => fn =>
    flatMap(parser1, v1 => 
      map(parser2, v2 => fn(v1, v2)))

  let product = (parser1, parser2) => map2(parser1, parser2, (v1, v2) => (v1, v2))

  let char = c => (char_to_string(c) |> string) |. map(s => s.[0])
};

module ParserInfix = (P: Parser, PS: Parsers) => {
  module Parsers = PS(P);
  module DP = DerivedParsers(P, PS)
  open Parsers;
  open DP;

  let (>>=) = flatMap
  let (<$>) = map
  let (>>) = product
  let (<|>) = orElse
}

module Location = {
  type t = {
    input: string,
    offset: int
  }
  let make = (input, offset) => { input, offset }
  let line = loc => {
    let countBreaks = str => List.fold_left((acc, v) => {
      if (v == '\n') {
        acc + 1
      } else {
        acc
      }
    }, 0, explode(str));
    countBreaks(String.sub(loc.input, 0, loc.offset+1)) + 1
  }

  let col = loc => 
    String.sub(loc.input, 0, loc.offset+1) 
    |> explode 
    |> List.rev 
    |> Array.of_list
    |> Js.Array.indexOf('\n')
}

module type ErrorReporting = (P: Parser) => {
  type parser('a) = P.parser('a);
  type parse_error = P.parse_error;
  let errorLocation: parse_error => Location.t
  let errorMessage: parse_error => string
}


/*module Parser = {

  type state = {
    input: list(char),
    line: int,
    column: int
  };

  type parser_result('a) = result(('a, state), (string, state));

  type t('a) = state => parser_result('a);

  let run_parser: t('a) => string => parser_result('a) = parser => input =>
    parser({
      input: explode(input),
      line: 0,
      column: 0
    });
};
  
module Parsers = {

  let char: char => Parser.t(char) = char => state => {
    switch (state.input) {
    | [ch, ...next_input] when char == ch => Ok((char, { ...state, input: next_input, column: state.column + 1 }))
    | [ch] => Err(("Expecting " ++ char_to_string(char) ++ ", but got " ++ char_to_string(ch), state))
    | _ => Err(("Couldn't find character " ++ char_to_string(char), state))
    }
  };

  let flatMap: Parser.t('a) => ('a => Parser.t('b)) => Parser.t('b) = parser => fn => state =>
    switch(parser(state)) {
    | Ok((v, state)) => fn(v, state)
    | Err((m, state)) => Err((m, state))
    }

  let (>>=) = flatMap
  
  let unit: 'a => Parser.t('a) = value => state => Ok((value, state))

  let map = parser => fn => flatMap(parser, v => unit(fn(v)))
  let (<$>) = map

  let map2 = (p1, p2) => fn =>
    p1 >>= v1 =>
    p2 <$> v2 => fn(v1, v2)

  let product = (p1, p2) => map2(p1, p2, (v1, v2) => (v1, v2))

  let orElse: Parser.t('a) => Parser.t('a) => Parser.t('a) = parserA => parserB => state =>
    switch (parserA(state)) {
    | Ok((v, state)) => Ok((v, state))
    | Err((m1, _)) =>
      switch (parserB(state)) {
      | Ok((v, state)) => Ok((v, state))
      | Err((m2, _)) => Err((m1 ++ "\n" ++ m2, state))
      } 
    };
  
  let empty: Parser.t('a) = s => Ok(("", s)) |> Obj.magic

  let reduce = (combine: (Parser.t('a), Parser.t('a)) => Parser.t('a), list) => List.fold_left(combine, empty, list)
  
  let choice = reduce(orElse)

  let anyOf = list_of_chars =>
    list_of_chars
    |> List.map(char)
    |> choice

  let (>>) = product
  let (<|>) = orElse

  let string = str => {
    explode(str)
    |> List.map(char)
    |> List.map(x => map(x, char_to_string))
    |> reduce((p1, p2) =>
      product(p1, p2) <$> ((a, b)) => a ++ b
    )
  }

  let listOfN: int => Parser.t('a) => Parser.t(list('a)) = n => parser => state => {
    let rec lofn = (n, parser, state, list) => {
      if (n == 0) {
        Ok((list, state))
      } else {
        switch (parser(state)) {
        | Ok((v, state)) => lofn(n-1, parser, state, list @ [v])
        | Err((m, state)) => Err((m, state))
        }
      }
    };
    lofn(n, parser, state, [])
  }

  let succeed = unit

  let many: Parser.t('a) => Parser.t(list('a)) = parser => state => {
    let rec mny = (parser, state, list) => {
      switch (parser(state)) {
      | Ok((v, state)) => mny(parser, state, list @ [v])
      | _ => Ok((list, state))
      }
    };
    mny(parser, state, [])
  };

  let many1 = parser =>
    map2(parser, many(parser), (first, results) => [first] @ results)

  let slice: Parser.t('a) => Parser.t(string) = parser => state => {
    switch (parser(state)) {
      | Ok((_, nextState)) =>
        /* compare both strings to check difference */
        let diff_length = List.length(state.input) - List.length(nextState.input);
        let slice = char_list_to_string(take(diff_length, state.input));
        Ok((slice, nextState)); 
      | Err((m, state)) => Err((m, state))
      }
  }

  let regex: Js.Re.t => Parser.t(string) = regex => state => {
    let input_string = char_list_to_string(state.input);
    let executed = Js.Re.exec(input_string, regex);
    let matched = Belt_Option.flatMap(executed, result => Js.Re.captures(result)[1] |> Js.Nullable.toOption);
    /* TODO: Fix state */
    switch(matched) {
    | Some(result) => Ok((result, state))
    | _ => Err(("Couldn't match regex" ++ (regex |> Obj.magic), state))
    }
  } 
};*/

module JSON = {
  
};

/*
open Parser;
open Parsers;

let parser = many(char('a')) >> char('b') |> slice

let result = run_parser(parser, "aaabcasd");

switch(result) {
| Ok((v)) => Js.log(v)
| Err((m)) => Js.log(m)
}*/



