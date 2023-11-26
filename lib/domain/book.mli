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
val empty : unit -> t

(**/**)

module Private : sig
  val create_book_created_event : book_id -> owner_id -> Isbn.t -> Pages.t -> e
  val create_book_deleted_event : book_id -> owner_id -> e
  val create_book_finished_event : book_id -> owner_id -> e
  val create_book_wanted_event : book_id -> owner_id -> e
  val create_book_quit_event : book_id -> owner_id -> e
  val create_read_to_page_event : book_id -> owner_id -> Pages.t -> Pages.t -> e
  val create_book_started_event : book_id -> owner_id -> e

  type p_book = {
    id : book_id;
    isbn : Isbn.t;
    owner_id : owner_id;
    total_pages : Pages.t;
  }

  val create_wanted_book : p_book -> t
  val create_reading_book : p_book -> Pages.t -> t
  val create_finished_book : p_book -> t
  val create_quit_book : p_book -> t
  val create_deleted_book : p_book -> t
end

(**/**)
