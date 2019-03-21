open Util;
module Parser = Scheme_Parser
module Interpreter = Scheme_Interpreter

let parse = Combinators.run(Parser.expr)
let eval = Interpreter.eval << Combinators.get_exn << parse
let show = Interpreter.showVal
