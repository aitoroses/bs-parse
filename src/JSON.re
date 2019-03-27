// https://gist.github.com/justjkk/436828/
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
    regex("\"([^\"]*)\"\s*:\s*") >>= captured => /* "(hello)" : <expr> */  
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

// Show
let rec show = json => switch json {
| JUndefined => "undefined";
| JNull => "null";
| JNumber(float) => Js.Float.toString(float);
| JString(string) => "\"" ++ string ++ "\"";
| JBool(bool) => if (bool) { "true" } else { "false" };
| JArray(array) => 
    open! Js.Array2;
    "[" ++ array->map(show)->joinWith(",") ++ "]";
| JObject(array) =>
    open! Js.Array2;
    let memberShow = ((key, json)) => {
        let value = show(json);
        {j|"$key":$value|j}
    };
    "{" ++ array->map(memberShow)->joinWith(",") ++ "}"
};