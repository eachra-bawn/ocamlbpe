let doc_string = "bpe -file <file> [-iter] [<iter_size>]"
let input_file = ref String.empty
let iter_size = ref 0
(* let anon_fun filename n =
  input_file := filename;
  iter_size := n *)

let speclist =
  [
    ("-iter", Arg.Set_int iter_size, "Set number of iterations to preform");
    ("-file", Arg.Set_string input_file, "Set file to byte-encode");
  ]

let () = Arg.parse speclist (fun _ -> ()) doc_string
