/*type token =
  | OpenParen
  | CloseParen
  | Number(string)
  | String(string)
  | Identifier(string);

let explode = str => {
  let rec exp = i => list =>
    if (i < 0) {
      list
    } else {
      exp(i - 1, [str.[i], ...list])
    };
  exp(String.length(str) - 1, [])
};

exception NoValidToken(string)

let str_of_char = c => String.make(1, c)

let tokenizer = input => {
  let rec tok = input => current => tokens => 
    switch (input) {
    | [] => List.rev(tokens)
    | [head, ...tail] =>
      let next = tok(tail); /* partial application */
      switch (head, current, tokens) {
      /* State: None */
      | ('(', None, t) => next(None, [OpenParen, ...t])
      | (')', None, t) => next(None, [CloseParen, ...t])
      | (' ' | '\t' | '\r' | '\n', None, t) => next(None, t)
      | ('"', None, t) => next(Some(String("")), t)
      | ('0'..'9' as i, None, t) => next(Some(Number(str_of_char(i))), t)
      | ('a'..'z' as i, None, t) => next(Some(Identifier(str_of_char(i))), t)
      /* State: String */
      | ('"', Some(String(s)), t) => next(None, [String(s), ...t])
      | (i, Some(String(s)), t) => next(Some(String(s ++ str_of_char(i))), t)
      /* State: Number */
      | ('0'..'9' as i, Some(Number(n)), t) => next(Some(Number(n ++ str_of_char(i))), t)
      | (')', Some(Number(n)), t) => next(None, [CloseParen, Number(n), ...t])
      | (' ', Some(Number(n)), t) => next(None, [Number(n), ...t])
      /* State: Identifier */
      | ('a'..'z' as i, Some(Identifier(n)), t) => next(Some(Identifier(n ++ str_of_char(i))), t)
      | (')', Some(Identifier(i)), t) => next(None, [CloseParen, Identifier(i), ...t])
      | (' ', Some(Identifier(i)), t) => next(None, [Identifier(i), ...t])
      | (c, _, _) => raise(NoValidToken(String.make(1, c)))
      }
    };
  tok(explode(input), None, [])
};

Js.log(tokenizer("(def a 1)"))
*/

/*module type Monoid = {
  type t
  let empty: t
  let concat: (t, t) => t
};

module Reduce = (M: Monoid) => {
  let reduce = (fn, list) => List.fold_left(M.concat, M.empty, list)
}*/

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

module Parser = {

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

  let map: ('a => 'b) => Parser.t('a) => Parser.t('b) = fn => parser => state => {
    switch(parser(state)) {
    | Ok((value, state)) => Ok((fn(value), state))
    | Err(e) => Err(e)
    }
  };

  let andThen: Parser.t('a) => Parser.t('b) => Parser.t(('a, 'b)) = parserA => parserB => state =>
    switch (parserA(state)) {
    | Ok((valueA, state)) =>
      switch (parserB(state)) {
      | Ok((valueB, state)) => Ok(((valueA, valueB), state))
      | Err(e) => Err(e)
      }
    | Err(e) => Err(e)  
    }

  let map2 = fn => (p1, p2) => map(((v1, v2)) => fn(v1, v2), andThen(p1, p2))

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

  let (<$>) = (x, f) => map(f, x)
  let (>>) = andThen
  let (<|>) = orElse

  let string = str => {
    explode(str)
    |> List.map(char)
    |> List.map(map(char_to_string))
    |> reduce((p1, p2) =>
      andThen(p1, p2) <$> ((a, b)) => a ++ b
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

  let succeed = a => string("") <$> _ => a

  let many: Parser.t('a) => Parser.t(list('a)) = parser => state => {
    let rec mny = (parser, state, list) => {
      switch (parser(state)) {
      | Ok((v, state)) => mny(parser, state, list @ [v])
      | _ => Ok((list, state))
      }
    };
    mny(parser, state, [])
  };

  let many1: Parser.t('a) => Parser.t(list('a)) = parser =>
    andThen(parser, many(parser)) <$> ((first, results)) => [first] @ results

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
};

module JSON = {
  
};


module Scheme = {

  type token =
    | OpenParen

  open Parsers;

  let line_break = char('\n')

  let open_paren = char('(')
  let close_paren = char(')')

};

open Parser;
open Parsers;

let parser = many(char('a')) >> char('b') |> slice

let result = run_parser(parser, "aaabcasd");

switch(result) {
| Ok((v)) => Js.log(v)
| Err((m)) => Js.log(m)
}



