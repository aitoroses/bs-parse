open Jest;

open Combinators;

type bools =
  | True
  | False

describe("Parsers", () => {
  open Expect;
  open! Expect.Operators;

  describe("errors", () => {
    test("should be nice", () => {
      let error = run(string("abx"), "abra") |> get_error;
      expect(error |> ParseError.getAllStackTrace) == [|"Expected: abx at line 1, column 1"|]
    })
  })

  describe("string", () => {

    test("success", () => {
      let result = run(string("abr"), "abra") ;
      expect(result |> get_exn) |> toBe("abr")
    })

    test("failure", () => {
      expect(() => run(string("abx"), "abra") |> get_exn) |> toThrow
    })
    
  })

  describe("orElse", () => {

    let p = orElse(string("aa"), string("bb"))

    test("success first branch", () => {
      let result = run(p, "aabra") ;
      expect(result |> get_exn) |> toBe("aa")
    })

    test("success second branch", () => {
      let result = run(p, "bbra") ;
      expect(result |> get_exn) |> toBe("bb")
    })

    test("failure", () => {
      let result = run(p, "abbra");
      expect(result |> get_error |> ParseError.getAllStackTrace) == [|
        "Expected: bb at line 1, column 1",
        "Expected: aa at line 1, column 1"
      |]
    })
  })

  describe("flatMap", () => {
    let p = flatMap(string("aa"), _ => string("bb"))

    test("success", () => {
      let result = run(p, "aabb") ;
      expect(result |> get_exn) |> toBe("bb")
    })

    test("failure", () => {
      let result = run(p, "aaabb") ;
      expect(result |> get_error |> ParseError.getAllStackTrace) == [|
        "Expected: bb at line 1, column 3",
      |]
    })
  })

  test("unit", () => {
    let result = run(unit(1), "abra"); // no matter the input
    expect(result |> get_exn) === 1
  })

  describe("many", () => {
    let p = many(string("aa"))

    test("aaaa", () => {
      expect(run(p, "aaaa") |> get_exn) == [|"aa", "aa"|]
    })

    test("aa", () => {
      expect(run(p, "aa") |> get_exn) == [|"aa"|]
    })

    test("no input", () => {
      expect(run(p, "") |> get_exn) == [||]
    })
  })

  describe("many1", () => {
    let p = many1(string("a"))

    test("success", () => {
      let result = run(p, "aabb") ;
      expect(result |> get_exn) == [|"a", "a"|]
    })

    test("failure", () => {
      let result = run(p, "bb") ;
      expect(result |> get_error |> ParseError.getAllStackTrace) == [|
        "Expected at least one repetition for parser at line 1, column 1",
      |]
    })
  })

  test("slice", () => {
    let p = many1(string("a")) |> slice
    let result = run(p, "aaabb");
    expect(result |> get_exn) == "aaa"
  })

  describe("regex", () => {

    test("simple", () => {  
      let p = regex("a(b)c?ra?")
      let result = run(p, "abrcaaaaa");
      expect(result |> get_exn) == [|"abr", "b"|]
    })

    test("continuation", () => {  
      let p = regex("a(b)c?ra?") >> string("c")
      let result = run(p, "abrcaaaaa");
      expect(result |> get_exn) == ([|"abr", "b"|], "c")
    })
  })

  describe("sepBy", () => {

    test("no repetition", () => {
        let json = {||}
        let result = run(sepBy(",", string("a")), json)
        expect(result |> get_exn) == [||]
    })

    test("one repetition", () => {
        let json = {|a|}
        let result = run(sepBy(",", string("a")), json)
        expect(result |> get_exn) == [|"a"|]
    })

    test("sepBy", () => {
        let json = {|a,a,a|}
        let result = run(sepBy(",", string("a")), json)
        expect(result |> get_exn) == [|"a", "a", "a"|]
    })

    test("sepBy with last sep", () => {
        let json = {|a,a,a,|}
        let result = run(sepBy(",", string("a")), json)
        expect(result |> get_exn) == [|"a", "a", "a"|]
    })

    /*test("sepBy kinds", () => {
      let trueBool = string("true") <$> _ => True
      let falseBool = string("false") <$> _ => False

      let parser = JSON.Parser.surround(JSON.Parser.whitespace, orElse(trueBool, falseBool), JSON.Parser.whitespace)

      let list = {|true, false, true , false|}
      let result = run(sepBy(",", parser), list)
      expect(result |> get_exn) == [|True, False, True, True|]
    })*/

  })
});
