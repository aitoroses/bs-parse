open Combinators
open CommonCombinators

type scheme =
    | Identifier(string)
    | Number(float)
    | String(string)
    | True
    | False
    | List(array(scheme))
    | ProcedureCall(string, array(scheme))

let trueBool = string("#t") <$> _ => True
let falseBool = string("#f") <$> _ => False
let quotedString = str <$> s => String(s)
let number = number <$> numberStr => Number(float_of_string(numberStr))
let identifier = regex("[^()][\S#]*") |> slice
let literal = 
    trueBool <|> 
    lazy falseBool <|>
    lazy quotedString <|>
    lazy number


let openParen = string("(")
let closeParen = string(")")
let listR = expr => sepBy(whitespace, expr)

let procedureCallR = expr => 
    openParen >>= _ =>
    whitespace >>= _ =>
    identifier |> slice >>= iden =>
    whitespace >>= _ =>
    listR(Lazy.force(expr)) >>= value =>
    whitespace >>= _ =>
    closeParen <$> _ => ProcedureCall(iden, value)

let surroundedListR = expr => 
    openParen >>= _ =>
    whitespace >>= _ =>
    listR(Lazy.force(expr)) >>= value =>
    whitespace >>= _ =>
    closeParen <$> _ => List(value)

let rec exprR = lazy (
    literal <|>
    lazy procedureCallR(exprR) <|>
    lazy surroundedListR(exprR)
)

let expr = spaceAround(Lazy.force(exprR))
