open Util;

type t = {
  input: string,
  offset: int,
};

let make = (input, offset) => {input, offset};
let line = loc => {
  let countBreaks = str =>
    List.fold_left(
      (acc, v) =>
        if (v == '\n') {
          acc + 1;
        } else {
          acc;
        },
      0,
      explode(str),
    );
  countBreaks(String.sub(loc.input, 0, loc.offset + 1)) + 1;
};

let col = loc => {
  open! Js.Array;
  open! Js.String;
  let lines = 
    String.sub(loc.input, 0, loc.offset + 1)
    |> split("\n")
    |> reverseInPlace;
  length(lines->unsafe_get(0));
}

let inc = loc => offset => {
  let newLoc = {
    ...loc,
    offset: loc.offset + offset
  }
  newLoc
}
