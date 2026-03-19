(* let create_vocab str =
  let rec create_vocab_impl str i acc =
    if i = String.length str then List.sort_uniq Stdlib.compare acc
    else
      match str.[i] with
      | ' ' -> create_vocab_impl str (i + 1) acc
      | _ -> create_vocab_impl str (i + 1) (str.[i] :: acc)
  in
  create_vocab_impl str 0 []

let print_vocab vocab =
  let pp_char ppf char = Format.fprintf ppf "%c" char in
  Format.printf "vocab: %a\n\n"
    (Format.pp_print_list
       ~pp_sep:(fun out () -> Format.fprintf out ", ")
       pp_char)
    vocab *)

(* let () = print_vocab (create_vocab (add_end_of_word_tokens input_string))
let () = Corpus.pretty_print (Corpus.create (add_end_of_word_tokens input_string))
let corpus_vals = add_end_of_word_tokens input_string |> Corpus.create
let tuplified_corpus_vals = Tuplified_corpus.tuplify corpus_vals
let () = Tuplified_corpus.pretty_print tuplified_corpus_vals
let pp_token_max fmt (x, y) = Format.fprintf fmt "most common token -> (%s, %s)\n" x y *)

let get_token_max tuplified_corpus =
  let rec get_token_max_impl tuplified_corpus n ~token_max ~token_max_count =
    if n = Tuplified_corpus.length tuplified_corpus - 1 then token_max
    else
      let tuplified_corpus_freqs =
        fst (Tuplified_corpus.split tuplified_corpus)
      in
      let tuplified_corpus_vals =
        snd (Tuplified_corpus.split tuplified_corpus)
      in
      let tuplified_corpus_unravelled =
        List.combine tuplified_corpus_freqs tuplified_corpus_vals
      in
      let all_tuplified_tokens = List.flatten tuplified_corpus_vals in
      if n > List.length all_tuplified_tokens - 1 then token_max
      else
        let current_tuplified_token =
          try List.nth all_tuplified_tokens n with
          | Failure _ -> raise (Failure "current_tuplified_token Failure")
          | Invalid_argument _ ->
              raise
                (Invalid_argument "current_tuplified_token Invalid_argument")
          | _ -> List.nth all_tuplified_tokens n
        in
        let current_token_count =
          List.fold_left
            (fun acc corpus ->
              if List.mem current_tuplified_token (snd corpus) then
                let filter =
                  List.filter
                    (fun t -> t = current_tuplified_token)
                    (snd corpus)
                in
                (fst corpus * List.length filter) + acc
              else acc)
            0 tuplified_corpus_unravelled
        in
        if current_token_count > token_max_count then
          get_token_max_impl tuplified_corpus (n + 1)
            ~token_max:current_tuplified_token
            ~token_max_count:current_token_count
        else
          get_token_max_impl tuplified_corpus (n + 1) ~token_max
            ~token_max_count
  in
  get_token_max_impl tuplified_corpus 0
    ~token_max:(Char.escaped ' ', Char.escaped ' ')
    ~token_max_count:0

(* printing most freq token tuple *)
(* let () =
  let token_max =
    add_end_of_word_tokens input_string
    |> Corpus.create
    |> Tuplified_corpus.tuplify
    |> get_token_max
  in
  pp_token_max Format.std_formatter token_max
;; *)

(* let () = Corpus.pretty_print (Corpus.create (add_end_of_word_tokens input_string)) *)
(* let pp_string ppf s = Format.fprintf ppf "%s" s *)

(* let pp_list list = *)
(*   Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_string list *)
(* ;; *)

(* let pp_option = function *)
(*   | None -> Printf.printf "option: None\n" *)
(*   | Some x -> Printf.printf "option: %d\n" x *)
(* ;; *)

(* let fst_token_max_index ~corpus_val ~token_max =
  let fst_token_max = fst token_max in
  let fst_index =
    match List.find_index (fun t -> t = fst_token_max) corpus_val with
    | Some i -> ref (Some i)
    | None ->
        failwith "Could not get the first index of the first token_max value"
  in
  let rec get_index cv i =
    if !i = None || cv = [] then None
    else
      let index =
        let index_ref = ref (Some 0) in
        while !index_ref != None do
          index_ref := !i
        done;
        Option.get !index_ref
      in
      let new_cv = List.drop index corpus_val in
      let snd_token_max = snd token_max in
      let snd_index =
        try List.nth cv index
        with Failure _ -> failwith "Couldn't get second token_max index"
      in
      if snd_index = snd_token_max then Some index else get_index new_cv
  in
  get_index corpus_val fst_index *)

let find_token_max_index ~(corpus_val : Corpus.Value.t) ~token_max =
  let fst_token_max = fst token_max in
  let rec loop ~(cv : Corpus.Value.t) ~original_cv ~acc =
    let fst_token_max_index =
      Corpus.Value.find_index ~f:(fun t -> t = fst_token_max) cv
    in
    if
      cv = Corpus.Value.empty
      || original_cv = Corpus.Value.empty
      || fst_token_max_index = None
    then None
    else
      let fst_token_max_index_plus_acc =
        let value = Option.get fst_token_max_index + acc in
        Some value
      in
      let fst_token_max_index_plus_acc_val =
        Option.get fst_token_max_index_plus_acc
      in
      let new_cv =
        Corpus.Value.drop (fst_token_max_index_plus_acc_val + 1) cv
      in
      let check_for_snd_token_max =
        Corpus.Value.nth original_cv (fst_token_max_index_plus_acc_val + 1)
      in
      let snd_token_max = snd token_max in
      if check_for_snd_token_max = snd_token_max then
        fst_token_max_index_plus_acc
      else
        let tokens_dropped = Corpus.Value.compare_lengths ~v1:cv ~v2:new_cv in
        loop ~cv:new_cv ~original_cv:cv ~acc:tokens_dropped
  in
  loop ~cv:corpus_val ~original_cv:corpus_val ~acc:0

let corpus_learner (corpus : Corpus.t) (token_max : string * string) =
  let corpus_freqs, corpus_vals = Corpus.split corpus in
  let fst_token_max_val = fst token_max in
  let snd_token_max_val = snd token_max in
  let find_fst_token_max_val cv =
    find_token_max_index ~corpus_val:cv ~token_max
  in
  let fst_token_max_index_val cv = Option.get (find_fst_token_max_val cv) in
  let search_snd_token_max_index cv =
    Corpus.Value.find_index
      ~f:(fun t -> t = Corpus.Value.nth cv (fst_token_max_index_val cv + 1))
      cv
  in
  let corpus_vals_w_token_max =
    Corpus.Value.list_filter
      ~f:(fun cv ->
        find_fst_token_max_val cv <> None
        && search_snd_token_max_index cv <> None)
      corpus_vals
  in
  let token_max_replacement = fst_token_max_val ^ snd_token_max_val in
  let replace_token_max_in_cv cv =
    let insert_token_max =
      Corpus.Value.mapi
        ~f:(fun i t ->
          if i = fst_token_max_index_val cv then token_max_replacement else t)
        cv
    in
    let old_snd_token_max_val_index =
      match
        Corpus.Value.find_index
          ~f:(fun t -> t = snd_token_max_val)
          insert_token_max
      with
      | None -> 80
      | Some i -> i
    in
    Corpus.Value.filteri
      ~f:(fun i _ -> i != old_snd_token_max_val_index)
      insert_token_max
  in
  let replace_tokens =
    List.fold_left
      (fun acc cv ->
        if List.mem cv corpus_vals_w_token_max then
          replace_token_max_in_cv cv :: acc
        else cv :: acc)
      [] corpus_vals
  in
  Corpus.combine corpus_freqs (List.rev replace_tokens)

let input_string_from_file file =
  In_channel.with_open_bin file In_channel.input_all

let input_string_from_stdin () = In_channel.input_line In_channel.stdin

let input_string =
  if !Cmd.input_file != String.empty then
    match Sys.file_exists !Cmd.input_file with
    | true -> input_string_from_file !Cmd.input_file
    | false ->
        raise
          (Invalid_argument "File name provided does not exist, or is invalid")
  else (
    Printf.printf "Input your text: ";
    Out_channel.flush Out_channel.stdout;
    let output_string = Option.get (input_string_from_stdin ()) in
    output_string)

let add_end_of_word_tokens input_string =
  let words = String.split_on_char ' ' input_string in
  List.fold_left (fun acc w -> (w ^ "_ ") ^ acc) "" words

let generate_corpus_stage =
  let iter_size = !Cmd.iter_size in
  let init_corpus = add_end_of_word_tokens input_string |> Corpus.create in
  let init_token_max =
    init_corpus |> Tuplified_corpus.tuplify |> get_token_max
  in
  let init_corpus_learned = corpus_learner init_corpus init_token_max in
  let rec generate_corpus_stage_impl corpus counter =
    if iter_size = 0 || iter_size = 1 then init_corpus_learned
    else if counter = iter_size then corpus
    else
      let current_token_max =
        corpus |> Tuplified_corpus.tuplify |> get_token_max
      in
      let corpus_learned = corpus_learner corpus current_token_max in
      generate_corpus_stage_impl corpus_learned (counter + 1)
  in
  generate_corpus_stage_impl init_corpus 0

let () =
  let generated_corpus = generate_corpus_stage in
  Corpus.pretty_print generated_corpus
