open Common
open Base

type book = {
  id : book_id;
  isbn : Isbn.t;
  owner_id : owner_id;
  total_pages : Pages.t;
}

type t =
  | Wanted of book
  | Reading of reading_book
  | Finished of book
  | DNF of book
  | Deleted of book

and reading_book = { book : book; page_number : Pages.t }

type e =
  | BookCreated of {
      id : book_id;
      owner_id : owner_id;
      isbn : Isbn.t;
      total_pages : Pages.t;
    }
  | BookDeleted of book_event
  | BookFinished of book_event
  | BookStarted of book_event
  | BookWanted of book_event
  | BookQuit of book_event
  | ReadToPage of book_event * Pages.t * Pages.t

and book_event = { id : book_id; owner_id : owner_id }

let evolve book e =
  let get_book_from_t book =
    match book with
    | Reading { book; _ } -> book
    | Wanted x | Finished x | DNF x | Deleted x -> x
  in
  let evolve_impl t e =
    let book = get_book_from_t t in
    match e with
    | BookStarted _ ->
        Reading { book; page_number = Option.value_exn (Pages.create 0) }
    | BookCreated { id; owner_id; isbn; total_pages } ->
        Wanted { id; owner_id; total_pages; isbn }
    | BookFinished _ -> Finished book
    | BookWanted _ -> Wanted book
    | BookQuit _ -> DNF book
    | ReadToPage (_, _, new_page) -> Reading { book; page_number = new_page }
    | BookDeleted _ -> Deleted book
  in
  List.fold_left evolve_impl book e

let start_reading = function
  | Wanted x | Finished x | DNF x | Deleted x ->
      Ok [ BookStarted { owner_id = x.owner_id; id = x.id } ]
  | Reading _ -> Error "Already reading book"

let finish_reading = function
  | Wanted x | Deleted x | Reading { book = x; _ } ->
      Ok [ BookFinished { owner_id = x.owner_id; id = x.id } ]
  | Finished _ -> Error "Already finished book"
  | DNF _ -> Error "This book was not finished"

let quit = function
  | Finished x | DNF x | Wanted x | Deleted x | Reading { book = x; _ } ->
      Ok [ BookQuit { owner_id = x.owner_id; id = x.id } ]

let mark_as_wanted = function
  | Finished x | DNF x | Wanted x | Deleted x | Reading { book = x; _ } ->
      Ok [ BookWanted { owner_id = x.owner_id; id = x.id } ]

(* Not very happy with this function the result application makes it more complex than necessary *)
let read_to_page book page =
  let open Containers.Result.Infix in
  let start_if_needed (book, events) =
    let result =
      match book with
      | Wanted _ | Deleted _ -> start_reading book
      | _ -> Ok events
    in
    result >>= fun events' -> Ok (evolve book events', events @ events')
  in
  let read_to_page_impl (book, events) =
    (match book with
    | DNF _ | Finished _ -> Error "Cannot change page number"
    | Reading { book = x; page_number = pn }
      when page <= pn || page > x.total_pages ->
        Error "Enter valid page number"
    | Reading { book = x; page_number = pn } ->
        Ok [ ReadToPage ({ owner_id = x.owner_id; id = x.id }, pn, page) ]
    | Wanted _ | Deleted _ -> Ok events)
    >>= fun events' -> Ok (evolve book events', events @ events')
  in
  let finish_book (book, events) =
    (match book with
    | Reading { book = x; page_number = pn } when x.total_pages = pn ->
        finish_reading book
    | _ -> Ok events)
    >>= fun events' -> Ok (events @ events')
  in
  start_if_needed (book, []) >>= read_to_page_impl >>= finish_book

let delete = function
  | Deleted _ -> Error "Aready deleted"
  | Finished x | DNF x | Wanted x | Reading { book = x; _ } ->
      Ok [ BookDeleted { owner_id = x.owner_id; id = x.id } ]

let create book_id owner_id isbn pages =
  Ok [ BookCreated { id = book_id; owner_id; isbn; total_pages = pages } ]
