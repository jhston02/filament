open Common

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
  | BookCreated of book_event
  | BookDeleted of book_event
  | BookFinished of book_event
  | BookStarted of book_event
  | BookWanted of book_event
  | BookQuit of book_event
  | ReadToPage of book_event * Pages.t * Pages.t

and book_event = { id : book_id; owner_id : owner_id }

let start_reading = function
  | Wanted x | Finished x | DNF x | Deleted x ->
      Ok [ BookStarted { owner_id = x.owner_id; id = x.id } ]
  | Reading _ -> Error "Already reading book"

let finish_reading = function
  | Wanted x |  Deleted x | Reading {book=x;_} ->
      Ok [ BookFinished { owner_id = x.owner_id; id = x.id } ]
  | Finished _ -> Error "Already finished book"
  | DNF _ -> Error "This book was not finished"

let quit = function
  | Finished x | DNF x | Wanted x |  Deleted x | Reading {book=x;_} ->
      Ok [ BookQuit { owner_id = x.owner_id; id = x.id } ]

let mark_as_wanted = function
  | Finished x | DNF x | Wanted x |  Deleted x | Reading {book=x;_} ->
      Ok [ BookWanted { owner_id = x.owner_id; id = x.id } ]

let read_to_page book page = 
  let open Containers.Result.Infix in
  let* r = match book with
            | DNF _ | Finished _ -> Error "Cannot change page number"
            | Reading {book = x; page_number = pn} when page <= pn || page > x.total_pages -> Error "Enter valid page number"
            | Reading {book = x; page_number = pn} -> Ok ({owner_id = x.owner_id; id = x.id}, pn, page)
            | Wanted x | Deleted x -> Ok({owner_id = x.owner_id; id = x.id}) in
  match r with
  | Reading 
  
  
  

