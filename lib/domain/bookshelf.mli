open Common

type t [@@deriving eq, show]
type e [@@deriving eq, show]

val create : bookshelf_id -> owner_id -> string -> (e list, string) result
val add_book : t -> book_id -> (e list, string) result
val remove_book : t -> book_id -> (e list, string) result
val delete : t -> (e list, string) result
val empty : t
val rename : t -> string -> (e list, string) result
val evolve : t -> e list -> t

module Private : sig
  val create_bookshelf_created_event : bookshelf_id -> owner_id -> string -> e
  val create_bookshelf_deleted_event : bookshelf_id -> owner_id -> e
  val create_book_added_event : bookshelf_id -> owner_id -> book_id -> e
  val create_book_removed_event : bookshelf_id -> owner_id -> book_id -> e
  val create_bookshelf_renamed_event : bookshelf_id -> owner_id -> string -> string -> e

  val create_bookshelf : bookshelf_id -> owner_id -> string -> bool -> t
  val create_bookshelf_with_book : bookshelf_id -> owner_id -> string -> book_id -> t
end
