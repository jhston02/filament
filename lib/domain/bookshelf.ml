open Common

type bookshelf = {
  id : bookshelf_id;
  owner_id : owner_id;
  books : book_id list;
  name : string;
}

type t = Deleted of bookshelf | In_use of bookshelf

type e = Bookshelf_created of bookshelf | Bookshelf_deleted of bookshelf_event
and bookshelf_event = { owner_id : owner_id; bookshelf_id : bookshelf_id }
