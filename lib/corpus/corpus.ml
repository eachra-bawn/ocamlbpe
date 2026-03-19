module Value = struct
  type t = string list
  let empty = []
  let length (v : t) = List.length v
  let compare_lengths ~(v1 : t) ~(v2 : t) = List.compare_lengths v1 v2
  let nth (v : t) n = List.nth v n
  let mapi ~(f) (v : t) : t = List.mapi f v
  let find_index ~f (v : t) = List.find_index f v
  let filter ~f (v : t) = List.filter f v
  let filteri ~f (v : t) = List.filteri f v
  let list_filter ~f (vl : t list) = List.filter f vl
  let exists ~f (v : t) = List.exists f v
  let flatten (t_list : t list) : t = List.flatten t_list
  let drop n (v : t) = List.drop n v
end

type t = (int * Value.t) list

let empty = []
let length (corpus : t) = List.length corpus
let rev (corpus : t) = List.rev corpus

let nth (corpus : t) (n : int) =
  if n < 0 then raise (Invalid_argument "`n` needs to be greater than or equal to 0")
  else let rec nth_aux c n =
  match c with
  | [] -> raise (Invalid_argument "Corpus cannot be empty")
  | hd :: tl -> if n = 0 then hd else nth_aux tl (n - 1)
  in
  nth_aux corpus n

let split (corpus : t) = List.split corpus
let combine int_list corpus_vals : t = List.combine int_list corpus_vals

let word_freq str n =
  let words = String.split_on_char ' ' str in
  let word = List.nth words n in
  let filter = List.filter (fun str -> str = word) words in
  List.length filter

let create str =
  let rec create_impl str n corpus =
    let words = String.split_on_char ' ' str in
    let word = List.nth words n in
    let word_tokenised =
      String.fold_left (fun acc c -> Char.escaped c :: acc) [] word
    in
    let token_freq = word_freq str n in
    let corpus_tuple = (token_freq, List.rev word_tokenised) in
    if n = List.length words - 1 then
      List.rev (List.sort_uniq Stdlib.compare corpus)
    else create_impl str (n + 1) (corpus_tuple :: corpus)
  in
  create_impl str 0 []

let pretty_print corpus =
  let pp_int ppf d = Format.fprintf ppf "%d" d in
  let pp_string ppf s = Format.fprintf ppf "%s" s in
  let pp_char_list lst =
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
       pp_string)
      lst
  in
  let pp_tuple ppf (x, y) =
    Format.fprintf ppf "(%a, %a)" pp_int x pp_char_list y
  in
  let pp_corpus crps =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n")
      pp_tuple crps
  in
  Printf.printf "\n";
  Printf.printf "Tokenized result:\n";
  Format.printf "@[<h>@;<0 2>%a@;<0 0>@]\n\n" pp_corpus corpus
