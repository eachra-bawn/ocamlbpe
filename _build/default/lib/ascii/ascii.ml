let needs_decode corpus =
  let _, vals = Corpus.split corpus in
  let vals_flattened = Corpus.Value.flatten vals in
  Corpus.Value.exists (fun t -> String.starts_with ~prefix:"/" t) vals_flattened

let ascii_decode corpus =
  let x  = Corpus.split corpus in
  let vals_flattened = List.flatten vals in
  let contains_ascii = List.filter (fun t -> needs_decode t) vals_flattened in
