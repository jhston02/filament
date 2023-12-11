open Common

type t = {
  id : bookshelf_id;
  owner_id : owner_id;
  books : book_id list;
  name : string;
  deleted : bool;
}
[@@deriving eq, show]

type e = Bookshelf_created of t | Bookshelf_deleted of bookshelf_event | Bookshelf_renamed of bookshelf_event * string | Book_added of bookshelf_event * book_id | Book_removed of bookshelf_event * book_id
and bookshelf_event = { owner_id : owner_id; bookshelf_id : bookshelf_id }
[@@deriving eq, show]

let empty = {
  id = Bookshelf_id "";
  owner_id = Owner_id "";
  books = [];
  name = "";
  deleted = false;
}

let create id owner_id name =
  let bookshelf = { id; owner_id; books=[]; name ; deleted=false} in
  Ok ([Bookshelf_created bookshelf])

let delete (bookshelf: t) =
  match bookshelf.deleted with
  | true -> Error "Bookshelf already deleted"
  | false -> Ok ([Bookshelf_deleted { owner_id = bookshelf.owner_id; bookshelf_id = bookshelf.id }])

let rename (bookshelf: t) name =
  match bookshelf.deleted with
  | true -> Error "Bookshelf already deleted"
  | false -> Ok ([Bookshelf_renamed ({ owner_id = bookshelf.owner_id; bookshelf_id = bookshelf.id }, name)])

let add_book (bookshelf: t) book_id =
  match bookshelf.deleted with
  | true -> Error "Bookshelf already deleted"
  | false when List.mem book_id bookshelf.books -> Error "Book already in bookshelf"
  | false -> Ok ([Book_added ({ owner_id = bookshelf.owner_id; bookshelf_id = bookshelf.id }, book_id)])

let remove_book (bookshelf: t) book_id =
  match bookshelf.deleted with
  | true -> Error "Bookshelf already deleted"
  | false when not (List.mem book_id bookshelf.books) -> Error "Book not in bookshelf"
  | false ->  Ok ([Book_removed ({ owner_id = bookshelf.owner_id; bookshelf_id = bookshelf.id }, book_id)])

let evolve bookshelf event =
  let evolve_impl bookshelf event =
    match event with
    | Bookshelf_created bookshelf -> bookshelf
    | Bookshelf_deleted _ -> { bookshelf with deleted = true }
    | Bookshelf_renamed _ -> bookshelf
    | Book_added (_, book_id) -> { bookshelf with books = book_id :: bookshelf.books }
    | Book_removed (_, book_id) -> { bookshelf with books = List.filter (fun id -> id <> book_id) bookshelf.books }
  in
  List.fold_left evolve_impl bookshelf event

  module Private = struct
    let create_bookshelf_created_event bookshelf_id owner_id name = Bookshelf_created { id = Bookshelf_id bookshelf_id; owner_id; books = []; name = name; deleted = false }
    let create_bookshelf_deleted_event bookshelf_id owner_id = Bookshelf_deleted {owner_id; bookshelf_id}
    let create_book_added_event bookshelf_id owner_id book_id = Book_added ({bookshelf_id; owner_id}, book_id)
    let create_book_removed_event bookshelf_id owner_id book_id = Book_removed ({bookshelf_id; owner_id}, book_id)
    let create_bookshelf_renamed_event bookshelf_id owner_id name = Bookshelf_renamed ({bookshelf_id; owner_id},name)
  
    let create_bookshelf bookshelf_id owner_id name deleted = {id=bookshelf_id; owner_id; name; deleted; books=[]}
  end
  