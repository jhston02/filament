open Common

type t
type e

val start_reading : t -> e list option
val finish_reading : t -> e list option
val quit : t -> e list option
val mark_as_wanted : t -> e list option
val read_to_page : t -> int -> e list option
val delete : t -> e list option
val evolve : t -> e -> t
val create : book_id -> owner_id -> Isbn.t -> Pages.t
