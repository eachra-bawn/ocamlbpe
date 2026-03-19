module Value : sig
  type t
  val empty : t
  val length : t -> int
  val compare_lengths : v1:t -> v2:t -> int
  val nth : t -> int -> string
  val mapi : f:(int -> string -> string) -> t -> t
  val find_index : f:(string -> bool) -> t -> int option
  val filter : f:(string -> bool) -> t -> t
  val filteri: f:(int -> string -> bool) -> t -> t
  val list_filter : f:(t -> bool) -> t list -> t list
  val exists : f:(string -> bool) -> t -> bool
  val flatten : t list -> t
  val drop : int -> t -> t
end

type t

val empty : 'a list
val length : t -> int
val rev : t -> t
val nth : t -> int -> int * Value.t
val split : t -> (int list * Value.t list)
val combine : int list -> Value.t list -> t

(* Gives the number of times a word appears in a string.
   Used in `create_corpus`. *)
val word_freq : string -> int -> int

(* Creates corpus *)
val create : string -> t

(* Pretty printing corpus *)
val pretty_print : t -> unit
