# bs-parse

A monadic parser combinator library

The Combinators module provides a basic set of combinators for parsing that can be composed in more complex parsers.


## Installation
```
npm install --save @glennsl/bs-json
```

Then add `bs-parse` to bs-dependencies in your bsconfig.json:
```
{
  ...
  "bs-dependencies": ["@glennsl/bs-json"]
}
```

## Documentation

In progress.

Please refer to `Scheme` and `Json` modules. They provide some easy reference implementations of parsers and evaluators.

Tests also illustrate some good usage examples.

## Example

### Json Parser

in 31 lines

```ocaml
open Combinators;
open CommonCombinators;
type json =
    | JUndefined
    | JNull
    | JNumber(float)
    | JString(string)
    | JBool(bool)
    | JArray(array(json))
    | JObject(array((string,json)));

let undefined = string("undefined") <$> _ => JUndefined
let null = string("null") <$> _ => JNull
let trueBool = string("true") <$> _ => JBool(true)
let falseBool = string("false") <$> _ => JBool(false)
let bools = trueBool <|> lazy falseBool
let quotedString = str <$> s => JString(s)
let number = number <$> numberStr => JNumber(float_of_string(numberStr))
let literal = undefined <|> lazy null <|> lazy bools <|> lazy quotedString <|> lazy number;
let objectMemberP = expr =>
    regex("\"([^\"]*)\"\s*:\s*") >>= captured =>  
    expr <$> value => {
        let key = captured->Array.get(1);
        (key, value)
    }
let objP = expr => surround(string("{"), sepBy(string(","))(objectMemberP(expr) |> spaceAround), string("}")) <$> res => JObject(res)
let arrayP = expr => surround(string("["), sepBy(string(","))(expr |> spaceAround), string("]")) <$> res => JArray(res)
let rec expr = lazy (literal <|> lazy objP(Lazy.force(expr)) <|> lazy arrayP(Lazy.force(expr)))
let objectMember = objectMemberP(Lazy.force(expr))
let obj = objP(Lazy.force(expr))
let array = arrayP(Lazy.force(expr))
```