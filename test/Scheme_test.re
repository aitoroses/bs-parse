open Jest;
open Expect;
open! Expect.Operators;
open Combinators;
open! CommonCombinators;

describe("Scheme", () => {

    test("number", () => {
        let json = {|1|}
        let result = json |> Scheme.parse
        expect(result |> get_exn) == Number(1.0)
    })

    test("string", () => {
        let json = {|"hello world"|}
        let result = json |> Scheme.parse
        expect(result |> get_exn) == String("hello world")
    })

    test("boolean", () => {
        let json = {|#f|}
        let result = json |> Scheme.parse
        expect(result |> get_exn) == False
    })

    test("list", () => {
        let json = {| (+ 1 2) |}
        let result = json |> Scheme.parse
        expect(result |> get_exn) == ProcedureCall("+", [|
            Number(1.0),
            Number(2.0)
        |])
    })

    test("if", () => {
        let json = {|
        (if #t
         (+ 1 2)
         (- 3 2))
        |}
        let result = json |> Scheme.parse
        expect(result |> get_exn) == ProcedureCall("if", [|
            True,
            ProcedureCall("+", [|
                Number(1.0),
                Number(2.0)
            |]),
            ProcedureCall("-", [|
                Number(3.0),
                Number(2.0)
            |])
        |])
    })

    test("EOF", () => {
        let json = {|
        (1))
        |}
        let result = json |> Scheme.parse
        expect(result |> get_error |> ParseError.getAllStackTrace) == [|
            "Expected EOF at line 2, column 12"
        |]
    })

})