open Common

type t [@@deriving eq, show]
type e [@@deriving eq, show]

val create : bookshelf_id -> owner_id -> string -> (e list, string) result
val add_book : t -> book_id -> (e list, string) result
val remove_book : t -> book_id -> (e list, string) result
val rename : t -> string -> (e list, string) result
val evolve : t -> e list -> t

module Private : sig
  val create_bookshelf_created_event : book_id -> owner_id -> Isbn.t -> Pages.t -> e
  val create_bookshelf_deleted_event : book_id -> owner_id -> e
  val create_book_added_event : book_id -> owner_id -> e
  val create_book_removed_event : book_id -> owner_id -> e
  val create_bookshelf_renamed_event : book_id -> owner_id -> e

  val create_bookshelf : bookshelf_id -> owner_id -> string -> bool -> t
end
