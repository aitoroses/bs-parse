
let code = "(if #t (+ 1 2) (- 3 2))"

let result = try(Scheme.eval(code)) {
| Scheme.Interpreter.RuntimeError(msg) => String((msg))
}

Js.log("The result is: " ++ Scheme.show(result))