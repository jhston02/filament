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
  | Book_create of {
      id : book_id;
      owner_id : owner_id;
      isbn : Isbn.t;
      total_pages : Pages.t;
    }
  | Book_deleted of book_event
  | Book_finished of book_event
  | Book_started of book_event
  | Book_wanted of book_event
  | Book_quit of book_event
  | Read_to_page of book_event * Pages.t * Pages.t
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
    | Empty, Book_create { id; owner_id; isbn; total_pages } ->
        Wanted { id; owner_id; total_pages; isbn }
    | _, _ -> (
        let book = get_book_from_t t in
        match e with
        | Book_started _ ->
            Reading { book; page_number = Option.get (Pages.create 1) }
        | Book_create _ -> failwith "Invalid state"
        | Book_finished _ -> Finished book
        | Book_wanted _ -> Wanted book
        | Book_quit _ -> DNF book
        | Read_to_page (_, _, new_page) ->
            Reading { book; page_number = new_page }
        | Book_deleted _ -> Deleted book)
  in
  List.fold_left evolve_impl book e

let start_reading = function
  | Wanted x | Finished x | DNF x | Deleted x ->
      Ok [ Book_started { owner_id = x.owner_id; id = x.id } ]
  | Reading _ -> Error "Already reading book"
  | Empty -> Error "Cannot read empty book"

let finish_reading = function
  | Wanted x | Deleted x | Reading { book = x; _ } ->
      Ok [ Book_finished { owner_id = x.owner_id; id = x.id } ]
  | Finished _ -> Error "Already finished book"
  | DNF _ -> Error "This book was not finished"
  | Empty -> Error "Cannot finish empty book"

let quit = function
  | Finished x | DNF x | Wanted x | Deleted x | Reading { book = x; _ } ->
      Ok [ Book_quit { owner_id = x.owner_id; id = x.id } ]
  | Empty -> Error "Cannot quit empty book"

let mark_as_wanted = function
  | Finished x | DNF x | Wanted x | Deleted x | Reading { book = x; _ } ->
      Ok [ Book_wanted { owner_id = x.owner_id; id = x.id } ]
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
        Ok [ Read_to_page ({ owner_id = x.owner_id; id = x.id }, pn, page) ]
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
  | Deleted _ -> Error "Already deleted"
  | Finished x | DNF x | Wanted x | Reading { book = x; _ } ->
      Ok [ Book_deleted { owner_id = x.owner_id; id = x.id } ]
  | Empty -> Error "Cannot delete empty book"

let create book_id owner_id isbn pages =
  Ok [ Book_create { id = book_id; owner_id; isbn; total_pages = pages } ]

module Private = struct
  type p_book = {
    id : book_id;
    isbn : Isbn.t;
    owner_id : owner_id;
    total_pages : Pages.t;
  }

  let create_book_created_event book_id owner_id isbn pages =
    Book_create { id = book_id; owner_id; isbn; total_pages = pages }

  let create_book_deleted_event book_id owner_id =
    Book_deleted { id = book_id; owner_id }

  let create_book_finished_event book_id owner_id =
    Book_finished { id = book_id; owner_id }

  let create_book_wanted_event book_id owner_id =
    Book_wanted { id = book_id; owner_id }

  let create_book_quit_event book_id owner_id =
    Book_quit { id = book_id; owner_id }

  let create_read_to_page_event book_id owner_id from to_page =
    Read_to_page ({ id = book_id; owner_id }, from, to_page)

  let create_book_started_event book_id owner_id =
    Book_started { id = book_id; owner_id }

  let create_wanted_book book =
    Wanted
      {
        id = book.id;
        owner_id = book.owner_id;
        isbn = book.isbn;
        total_pages = book.total_pages;
      }

  let create_reading_book book pages =
    let (x : book) =
      {
        id = book.id;
        owner_id = book.owner_id;
        isbn = book.isbn;
        total_pages = book.total_pages;
      }
    in
    Reading { book = x; page_number = pages }

  let create_finished_book book =
    Finished
      {
        id = book.id;
        owner_id = book.owner_id;
        isbn = book.isbn;
        total_pages = book.total_pages;
      }

  let create_quit_book book =
    DNF
      {
        id = book.id;
        owner_id = book.owner_id;
        isbn = book.isbn;
        total_pages = book.total_pages;
      }

  let create_deleted_book book =
    Deleted
      {
        id = book.id;
        owner_id = book.owner_id;
        isbn = book.isbn;
        total_pages = book.total_pages;
      }
end
