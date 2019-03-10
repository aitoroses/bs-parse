open Jest;
open Expect;
open! Expect.Operators;
open Combinators;

describe("JSON parser", () => {

    test("surround", () => {
        let surround = openP => bodyP => closeP => 
            openP >>= _ =>
            JSON.Parser.whitespace >>= _ =>
            bodyP >>= result =>
            JSON.Parser.whitespace >>= _ =>
            closeP <$> _ => result

        let json = {|[     a  ]|}
        let result = run(surround(string("["), string("a"), string("]")), json)
        expect(result |> get_exn) == "a"
    })

    test("null", () => {
        let json = {|null|}

        let result = run(JSON.Parser.null, json)
        expect(result |> get_exn) === JSON.JNull
    })


    test("undefined", () => {
        let json = {|undefined|}

        let result = run(JSON.Parser.undefined, json)
        expect(result |> get_exn) === JSON.JUndefined
    })

    test("string", () => {
        let json = {| "what"|}

        let result = run(JSON.Parser.whitespace >>= _ => JSON.Parser.quotedString, json)
        expect(result |> get_exn) == JSON.JString("what")
    })

    test("bool: true", () => {
        let json = {|true|}

        let result = run(JSON.Parser.bools, json)
        expect(result |> get_exn) == JSON.JBool(true)
    })

    test("bool: false", () => {
        let json = {|false|}

        let result = run(JSON.Parser.bools, json)
        expect(result |> get_exn) == JSON.JBool(false)
    })

    describe("number", () => {

        test("integer", () => {
            let json = {|1|}
            let result = run(JSON.Parser.number, json)
            expect(result |> get_exn) == JSON.JNumber(1.0)
        })

        test("float", () => {
            let json = {|1.1|}
            let result = run(JSON.Parser.number, json)
            expect(result |> get_exn) == JSON.JNumber(1.1)
        })

        test("signed", () => {
            let json = {|-1.1|}
            let result = run(JSON.Parser.number, json)
            expect(result |> get_exn) == JSON.JNumber(-1.1)
        })
    })

    describe("literal", () => {
        test("null", () => {
            let json = {|null|}

            let result = run(JSON.Parser.literal, json)
            expect(result |> get_exn) === JSON.JNull
        })


        test("undefined", () => {
            let json = {|undefined|}

            let result = run(JSON.Parser.literal, json)
            expect(result |> get_exn) === JSON.JUndefined
        })

        test("string", () => {
            let json = {|"what"|}

            let result = run(JSON.Parser.literal, json)
            expect(result |> get_exn) == JSON.JString("what")
        })

        test("bool: true", () => {
            let json = {|true|}

            let result = run(JSON.Parser.literal, json)
            expect(result |> get_exn) == JSON.JBool(true)
        })

        test("bool: false", () => {
            let json = {|false|}

            let result = run(JSON.Parser.literal, json)
            expect(result |> get_exn) == JSON.JBool(false)
        })

        test("number", () => {
            let json = {|01.1|}

            let result = run(JSON.Parser.literal, json)
            expect(result |> get_exn) == JSON.JNumber(1.1)
        })
        
    })

    describe("array", () => {

        test("empty array", () => {
            let json = {|[]|}
            let result = run(JSON.Parser.array, json)
            expect(result |> get_exn) == JSON.JArray([||])
        })

        test("one member", () => {
            let json = {|["hello"]|}
            let result = run(JSON.Parser.array, json)
            expect(result |> get_exn) == JSON.JArray([|JSON.JString("hello")|])
        })

        test("more members", () => {
            let json = {|["hello",true]|}
            let result = run(JSON.Parser.array, json)
            expect(result |> get_exn) == JSON.JArray([|JSON.JString("hello"), JSON.JBool(true)|])
        })

        test("space between", () => {
            let json = {|[    "hello"   ,    true   ]|}
            let result = run(JSON.Parser.array, json)
            expect(result |> get_exn) == JSON.JArray([|JSON.JString("hello"), JSON.JBool(true)|])
        })
    })
    
    describe("object", () => {
        test("empty object", () => {
            let json = {|{}|}
            let result = run(JSON.Parser.obj, json)
            expect(result |> get_exn) == JSON.JObject([||])
        })

        test("member expression", () => {
            let json = {|"hello" : "world"|}
            let result = run(JSON.Parser.objectMember, json)
            expect(result |> get_exn) == ("hello", JSON.JString("world"))
        })

        test("one member object", () => {
            let json = {|{ "hello" : "world" }|}
            let result = run(JSON.Parser.obj, json)
            expect(result |> get_exn) == JSON.JObject([|("hello", JSON.JString("world"))|])
        })

        test("one member object 2", () => {
            let json = {|{ "one": 1 }|}
            let result = run(JSON.Parser.obj, json)
            expect(result |> get_exn) == JSON.JObject([|("one", JSON.JNumber(1.0))|])
        })

        test("two member object", () => {
            let json = {|{ "hello" : "world", "one": 1 }|}
            let result = run(JSON.Parser.obj, json)
            expect(result |> get_exn) == JSON.JObject([|
                ("hello", JSON.JString("world")),
                ("one", JSON.JNumber(1.0))
            |])
        })

        test("two member withObject", () => {
            let json = {|{ "hello" : "world", "one": 1 }|}
            let result = run(JSON.Parser.obj, json)
            expect(result |> get_exn) == JSON.JObject([|
                ("hello", JSON.JString("world")),
                ("one", JSON.JNumber(1.0))
            |])
        })
    })

})