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
    let bools = orElse(trueBool, falseBool)
    let str = regex("\"([^\"]*)\"") <$> matches => Array.get(matches, 1)
    let quotedString = str <$> s => JString(s)
    let number = (regex("[-+]?[0-9]*\.?[0-9]+") |> slice) <$> numberStr => JNumber(float_of_string(numberStr))
    let literal = undefined <|> null <|> bools <|> quotedString <|> number


    let spaceAround = bodyP =>
        whitespace >>= _ =>
        bodyP >>= result =>
        whitespace <$> _ => result

    let surround = openP => bodyP => closeP => 
        openP >>= _ =>
        spaceAround(bodyP) >>= result =>
        closeP <$> _ => result

    let objectMember = 
        regex("\"([^\"]*)\"\s*:\s*") >>= captured =>
        literal <$> lit => (captured->Array.get(1), lit)

    let obj = surround(string("{"), sepBy(",")(objectMember |> spaceAround), string("}")) <$> res => JObject(res)

    let array = surround(string("["), sepBy(",")(literal |> spaceAround), string("]")) <$> res => JArray(res)

    let expr = literal <|> obj <|> array
}