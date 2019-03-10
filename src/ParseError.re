type t = {
    stack: array((Location.t, string)),
    otherFailures: array(t)
  }
  let makeWith = (stack, failures) => { stack: stack, otherFailures: failures }
  let make = loc => string => makeWith([|(loc, string)|], [||])

  let getStackTrace = error => Array.fold_left((acc, (loc, message)) => {
      let line = Location.line(loc)
      let column = Location.col(loc)
      let finalMessage = {j|$message at line $line, column $column|j}
      let res = Js.Array.concat([|finalMessage|], acc)
      res
    }, [||], error.stack)

  let getAllStackTrace = error => {
    let otherFailureStackTraces = 
      error.otherFailures 
      |> Array.map(getStackTrace) 
      |> Array.fold_left(Array.append, [||])
    Array.append(getStackTrace(error), otherFailureStackTraces)
  }

  let toString = error => error |> getAllStackTrace |> Js.Array.joinWith("\n")