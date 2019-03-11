open Jest;
open Expect;
open! Expect.Operators;
open Combinators;
open! CommonCombinators;
open Scheme;

describe("Scheme", () => {

    test("number", () => {
        let json = {|1|}
        let result = run(Scheme.expr, json)
        expect(result |> get_exn) == Number(1.0)
    })

    test("string", () => {
        let json = {|"hello world"|}
        let result = run(Scheme.expr, json)
        expect(result |> get_exn) == String("hello world")
    })

    test("boolean", () => {
        let json = {|#f|}
        let result = run(Scheme.expr, json)
        expect(result |> get_exn) == False
    })

    Only.test("list", () => {
        let json = {| (+ 1 2) |}
        let result = run(Scheme.expr, json)
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
        let result = run(Scheme.expr, json)
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

})