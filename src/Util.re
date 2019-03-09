
type result('a, 'b) =
  | Ok('a)
  | Err('b);

let explode = str => {
  let rec exp = i => list =>
    if (i < 0) {
      list
    } else {
      exp(i - 1, [str.[i], ...list])
    };
  exp(String.length(str) - 1, [])
};

let char_to_string = String.make(1)

let char_list_to_string = list => List.fold_left((++), "") @@ List.map(char_to_string) @@ list

let rec take = (n, list) =>
  switch (list) {
  | [] => []
  | [x, ...xs] => n == 0 ? [] : [x, ...take(n - 1, xs)]
  };