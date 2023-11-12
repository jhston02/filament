open Common

type t [@@deriving eq, show]
type e [@@deriving eq, show]

val start_reading : t -> (e list, string) result
val finish_reading : t -> (e list, string) result
val quit : t -> (e list, string) result
val mark_as_wanted : t -> (e list, string) result
val read_to_page : t -> Pages.t -> (e list, string) result
val delete : t -> (e list, string) result
val evolve : t -> e list -> t
val create : book_id -> owner_id -> Isbn.t -> Pages.t -> (e list, string) result
