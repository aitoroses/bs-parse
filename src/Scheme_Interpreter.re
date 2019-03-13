open Scheme_Parser;

let rec showVal = lisp => switch(lisp) {
| Atom(name) => name
| Number(content) => string_of_float(content)
| String(contents) => "\"" ++ contents ++ "\""
| True => "#t"
| False => "#f"
| List(xs) => "(" ++ (xs |> Js.Array.map(showVal) |> Js.Array.joinWith(" ")) ++ ")"
| ProcedureCall(name, xs) => "(" ++ name ++ " " ++ (xs |> Js.Array.map(showVal) |> Js.Array.joinWith(" ")) ++ ")"
}

exception RuntimeError(string)

let binNumOp = impl => params => switch(params) {
| [|Number(a), Number(b)|] => Number(impl(a, b))
| [|Number(_), b|] => raise(RuntimeError(showVal(b) ++ " must be a number"))
| [|a, Number(_)|] => raise(RuntimeError(showVal(a) ++ " must be a number"))
| _ => raise(RuntimeError(showVal(List(params)) ++ " must be binary number list"))
}

let if_ = eval => params => switch(params) {
| [|True, a, _|] => eval(a)
| [|False, _, b|] => eval(b)
| _ => raise(RuntimeError(showVal(List(params)) ++ " must be"))
}

let rec eval = lisp => switch(lisp) {
| ProcedureCall(name, params) => 
    switch(name) {
    | "+" => binNumOp((a, b) => a +. b, params) 
    | "-" => binNumOp((a, b) => a -. b, params) 
    | "if" => if_(eval, params)
    | op => raise(RuntimeError(op ++ " is not valid procedure"))
    }
| x => x
}