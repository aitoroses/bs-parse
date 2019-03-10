open Util;

module type Parsers = {
  type parser('a)
  type parse_error = ParseError.t
  type parse_result('a)
  let run: parser('a) => string => parse_result('a)
  let get_exn: parse_result('a) => 'a
  let get_error: parse_result('a) => parse_error
  let string: string => parser(string)
  let orElse: (parser('a), Lazy.t(parser('a))) => parser('a)
  let flatMap: parser('a) => ('a => parser('b)) => parser('b)
  let unit: 'a => parser('a)
  let listOfN: int => parser('a) => parser(array('a))
  let many: parser('a) => parser(array('a))
  let many1: parser('a) => parser(array('a))
  let slice: parser('a) => parser(string)
  let regex: string => parser(array(string))
  let sepBy: string => parser('a) => parser(array('a))

  /* seq(p)(f): Run a parser, then use its result to select a second parser to run in sequence 
   * for me this one is flatMap
   */
  
  let label: string => parser('a) => parser('a)
  let scope: string => parser('a) => parser('a)

  let attempt: parser('a) => parser(option('a))
  let fail: string => parser('a)

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
  module Derived_Parsers = DerivedParsers(PS);
  open PS;
  open Derived_Parsers;

  let (>>=) = flatMap
  let (<$>) = map
  let (>>) = product
  let (<|>) = orElse
}
