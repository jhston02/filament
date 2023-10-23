open Common

type t
type e

val start_reading : t -> (e list, string) result
val finish_reading : t -> (e list, string) result
val quit : t -> (e list, string) result
val mark_as_wanted : t -> (e list, string) result
val read_to_page : t -> Pages.t -> (e list, string) result
val delete : t -> (e list, string) result
val evolve : t -> e -> t
val create : book_id -> owner_id -> Isbn.t -> Pages.t -> (e list, string) result
