open Common

type t
type e

val create : bookshelf_id -> owner_id -> (e list, string) result
val add_book : t -> book_id -> (e list, string) result
val remove_book : t -> book_id -> (e list, string) result
val rename : t -> string -> (e list, string) result
val evolve : t -> e list -> t

module Private : sig
  type t
end
