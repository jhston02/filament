open Common

type book = {
  id : book_id;
  isbn : Isbn.t;
  owner_id : owner_id;
  total_pages : Pages.t;
}
[@@deriving eq, show]

type t =
  | Wanted of book
  | Reading of reading_book
  | Finished of book
  | DNF of book
  | Deleted of book
  | Empty
[@@deriving eq, show]

and reading_book = { book : book; page_number : Pages.t } [@@deriving eq, show]

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
[@@deriving eq, show]

and book_event = { id : book_id; owner_id : owner_id } [@@deriving eq, show]

let empty () = Empty

let evolve book e =
  let get_book_from_t book =
    match book with
    | Reading { book; _ } -> book
    | Wanted x | Finished x | DNF x | Deleted x -> x
    | Empty -> failwith "Cannot get book from Empty"
  in
  let evolve_impl t e =
    match (t, e) with
    | Empty, BookCreated { id; owner_id; isbn; total_pages } ->
        Wanted { id; owner_id; total_pages; isbn }
    | _, _ -> (
        let book = get_book_from_t t in
        match e with
        | BookStarted _ ->
            Reading { book; page_number = Option.get (Pages.create 0) }
        | BookCreated _ -> failwith "Invalid state"
        | BookFinished _ -> Finished book
        | BookWanted _ -> Wanted book
        | BookQuit _ -> DNF book
        | ReadToPage (_, _, new_page) ->
            Reading { book; page_number = new_page }
        | BookDeleted _ -> Deleted book)
  in
  List.fold_left evolve_impl book e

let start_reading = function
  | Wanted x | Finished x | DNF x | Deleted x ->
      Ok [ BookStarted { owner_id = x.owner_id; id = x.id } ]
  | Reading _ -> Error "Already reading book"
  | Empty -> Error "Cannot read empty book"

let finish_reading = function
  | Wanted x | Deleted x | Reading { book = x; _ } ->
      Ok [ BookFinished { owner_id = x.owner_id; id = x.id } ]
  | Finished _ -> Error "Already finished book"
  | DNF _ -> Error "This book was not finished"
  | Empty -> Error "Cannot finish empty book"

let quit = function
  | Finished x | DNF x | Wanted x | Deleted x | Reading { book = x; _ } ->
      Ok [ BookQuit { owner_id = x.owner_id; id = x.id } ]
  | Empty -> Error "Cannot quit empty book"

let mark_as_wanted = function
  | Finished x | DNF x | Wanted x | Deleted x | Reading { book = x; _ } ->
      Ok [ BookWanted { owner_id = x.owner_id; id = x.id } ]
  | Empty -> Error "Cannot want empty book"

(* Not very happy with this function the result application makes it more complex than necessary *)
let read_to_page book page =
  let open Containers.Result in
  let* started =
    match book with Wanted _ | Deleted _ -> start_reading book | _ -> Ok []
  in
  let started_book = evolve book started in
  let* result =
    match started_book with
    | DNF _ | Finished _ -> Error "Cannot change page number"
    | Reading { book = x; page_number = pn }
      when page <= pn || page > x.total_pages ->
        Error "Enter valid page number"
    | Reading { book = x; page_number = pn } ->
        Ok [ ReadToPage ({ owner_id = x.owner_id; id = x.id }, pn, page) ]
    | Wanted _ | Deleted _ -> Ok []
    | Empty -> Error "Cannot read empty book"
  in
  let result_book = evolve started_book result in
  let* finish =
    match result_book with
    | Reading { book = x; page_number = pn } when x.total_pages = pn ->
        finish_reading book
    | _ -> Ok []
  in
  Ok (started @ result @ finish)

let delete = function
  | Deleted _ -> Error "Aready deleted"
  | Finished x | DNF x | Wanted x | Reading { book = x; _ } ->
      Ok [ BookDeleted { owner_id = x.owner_id; id = x.id } ]
  | Empty -> Error "Cannot delete empty book"

let create book_id owner_id isbn pages =
  Ok [ BookCreated { id = book_id; owner_id; isbn; total_pages = pages } ]
