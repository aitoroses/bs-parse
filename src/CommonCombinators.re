open Combinators;

let whitespace = regex("[\s]*") |> slice <$> _ => ()

let spaceAround = bodyP =>
        whitespace >>= _ =>
        bodyP >>= result =>
        whitespace <$> _ => result

let surround = openP => bodyP => closeP => 
    openP >>= _ =>
    spaceAround(bodyP) >>= result =>
    closeP <$> _ => result;

let number = regex("-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?") |> slice

let str = regex("\"([^\"]*)\"") <$> matches => Array.get(matches, 1)
