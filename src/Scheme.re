module Parser = Scheme_Parser
module Interpreter = Scheme_Interpreter


let eval = code =>
    code 
    |> Combinators.run(Parser.expr) 
    |> Combinators.get_exn
    |> Interpreter.eval

let show = Interpreter.showVal
