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
