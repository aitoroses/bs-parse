type json =
    | JUndefined
    | JNull
    | JNumber(float)
    | JString(string)
    | JBool(bool)
    | JArray(array(json))
    | JObject(array((string,json)));

module Parser = {
    open Combinators;

    let whitespace = regex("[\s]*") |> slice <$> _ => ()

    let undefined = string("undefined") <$> _ => JUndefined
    let null = string("null") <$> _ => JNull
    let trueBool = string("true") <$> _ => JBool(true)
    let falseBool = string("false") <$> _ => JBool(false)
    let bools = orElse(trueBool, lazy falseBool)
    let str = regex("\"([^\"]*)\"") <$> matches => Array.get(matches, 1)
    let quotedString = str <$> s => JString(s)
    let number = (regex("[-+]?[0-9]*\.?[0-9]+") |> slice) <$> numberStr => JNumber(float_of_string(numberStr))
    let literal = undefined <|> lazy null <|> lazy bools <|> lazy quotedString <|> lazy number

    let spaceAround = bodyP =>
        whitespace >>= _ =>
        bodyP >>= result =>
        whitespace <$> _ => result

    let surround = openP => bodyP => closeP => 
        openP >>= _ =>
        spaceAround(bodyP) >>= result =>
        closeP <$> _ => result;

    module RecursiveParsers = {
        let objectMemberP = expr =>
            regex("\"([^\"]*)\"\s*:\s*") >>= captured =>
            expr <$> lit => (captured->Array.get(1), lit)

        let objP = expr => surround(string("{"), sepBy(",")(objectMemberP(expr) |> spaceAround), string("}")) <$> res => JObject(res)
        let arrayP = expr => surround(string("["), sepBy(",")(expr |> spaceAround), string("]")) <$> res => JArray(res)
    }

    open RecursiveParsers
    
    let rec expr = () => literal <|> lazy objP(expr()) <|> lazy arrayP(expr())

    let objectMember = objectMemberP(expr())
    let obj = objP(expr())
    let array = arrayP(expr())
}