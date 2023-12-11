open Filament

let book_events = Alcotest.testable Bookshelf.pp_e Bookshelf.equal_e
let book = Alcotest.testable Bookshelf.pp Bookshelf.equal

let bookshelf = {
  id = Bookshelf_id "id";
  owner_id = Owner_id "OId";
  books = [];
  name = "My Bookshelf";
  deleted = false;
}

let test__create__valid () =
  let open Common in
  let id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let name = "My Bookshelf" in
  let expected = Ok [Bookshelf_created { id; owner_id; books=[]; name; deleted=false }] in
  let result = Bookshelf.create id owner_id name in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookshelfCreated is same" expected result

let test__delete__valid () =
  let expected = Ok [Bookshelf_deleted { owner_id = Owner_id "OId"; bookshelf_id = Bookshelf_id "id" }] in
  let result = Bookshelf.delete bookshelf in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookshelfDeleted is same" expected result

let test__delete__already_deleted__invalid () =
  let deleted_bookshelf = { bookshelf with deleted = true } in
  let expected = Error "Bookshelf already deleted" in
  let result = Bookshelf.delete deleted_bookshelf in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookshelfDeleted is same" expected result

let test__rename__valid () =
  let name = "New Bookshelf Name" in
  let expected = Ok [Bookshelf_renamed { owner_id = Owner_id "OId"; bookshelf_id = Bookshelf_id "id" }] in
  let result = Bookshelf.rename bookshelf name in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookshelfRenamed is same" expected result

let test__rename__already_deleted__invalid () =
  let deleted_bookshelf = { bookshelf with deleted = true } in
  let name = "New Bookshelf Name" in
  let expected = Error "Bookshelf already deleted" in
  let result = Bookshelf.rename deleted_bookshelf name in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookshelfRenamed is same" expected result

let test__add_book__valid () =
  let book_id = Book_id "book_id" in
  let expected = Ok [Book_added ({ owner_id = Owner_id "OId"; bookshelf_id = Bookshelf_id "id" }, book_id)] in
  let result = Bookshelf.add_book bookshelf book_id in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookAdded is same" expected result

let test__add_book__already_deleted__invalid () =
  let deleted_bookshelf = { bookshelf with deleted = true } in
  let book_id = Book_id "book_id" in
  let expected = Error "Bookshelf already deleted" in
  let result = Bookshelf.add_book deleted_bookshelf book_id in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookAdded is same" expected result

let test__add_book__already_in_bookshelf__invalid () =
  let book_id = Book_id "book_id" in
  let bookshelf_with_book = { bookshelf with books = [book_id] } in
  let expected = Error "Book already in bookshelf" in
  let result = Bookshelf.add_book bookshelf_with_book book_id in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookAdded is same" expected result

let test__remove_book__valid () =
  let book_id = Book_id "book_id" in
  let bookshelf_with_book = { bookshelf with books = [book_id] } in
  let expected = Ok [Book_removed ({ owner_id = Owner_id "OId"; bookshelf_id = Bookshelf_id "id" }, book_id)] in
  let result = Bookshelf.remove_book bookshelf_with_book book_id in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookRemoved is same" expected result

let test__remove_book__already_deleted__invalid () =
  let deleted_bookshelf = { bookshelf with deleted = true } in
  let book_id = Book_id "book_id" in
  let expected = Error "Bookshelf already deleted" in
  let result = Bookshelf.remove_book deleted_bookshelf book_id in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookRemoved is same" expected result

let test__remove_book__not_in_bookshelf__invalid () =
  let book_id = Book_id "book_id" in
  let expected = Error "Book not in bookshelf" in
  let result = Bookshelf.remove_book bookshelf book_id in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookRemoved is same" expected result

let test__evolve__bookshelf_created () =
  let event = Bookshelf_created bookshelf in
  let expected = bookshelf in
  let result = Bookshelf.evolve Bookshelf.empty [event] in
  Alcotest.(check bookshelf) "Bookshelf is same" expected result

let test__evolve__bookshelf_deleted () =
  let event = Bookshelf_deleted { owner_id = Owner_id "OId"; bookshelf_id = Bookshelf_id "id" } in
  let expected = { bookshelf with deleted = true } in
  let result = Bookshelf.evolve bookshelf [event] in
  Alcotest.(check bookshelf) "Bookshelf is same" expected result

let test__evolve__bookshelf_renamed () =
  let event = Bookshelf_renamed { owner_id = Owner_id "OId"; bookshelf_id = Bookshelf_id "id" } in
  let expected = bookshelf in
  let result = Bookshelf.evolve bookshelf [event] in
  Alcotest.(check bookshelf) "Bookshelf is same" expected result

let test__evolve__book_added () =
  let event = Book_added ({ owner_id = Owner_id "OId"; bookshelf_id = Bookshelf_id "id" }, Book_id "book_id") in
  let expected = { bookshelf with books = [Book_id "book_id"] } in
  let result = Bookshelf.evolve bookshelf [event] in
  Alcotest.(check bookshelf) "Bookshelf is same" expected result

let test__evolve__book_removed () =
  let event = Book_removed ({ owner_id = Owner_id "OId"; bookshelf_id = Bookshelf_id "id" }, Book_id "book_id") in
  let expected = { bookshelf with books = [] } in
  let result = Bookshelf.evolve bookshelf [event] in
  Alcotest.(check bookshelf) "Bookshelf is same" expected result

let tests = [
  "test__create__valid", `Quick, test__create__valid;
  "test__delete__valid", `Quick, test__delete__valid;
  "test__delete__already_deleted__invalid", `Quick, test__delete__already_deleted__invalid;
  "test__rename__valid", `Quick, test__rename__valid;
  "test__rename__already_deleted__invalid", `Quick, test__rename__already_deleted__invalid;
  "test__add_book__valid", `Quick, test__add_book__valid;
  "test__add_book__already_deleted__invalid", `Quick, test__add_book__already_deleted__invalid;
  "test__add_book__already_in_bookshelf__invalid", `Quick, test__add_book__already_in_bookshelf__invalid;
  "test__remove_book__valid", `Quick, test__remove_book__valid;
  "test__remove_book__already_deleted__invalid", `Quick, test__remove_book__already_deleted__invalid;
  "test__remove_book__not_in_bookshelf__invalid", `Quick, test__remove_book__not_in_bookshelf__invalid;
  "test__evolve__bookshelf_created", `Quick, test__evolve__bookshelf_created;
  "test__evolve__bookshelf_deleted", `Quick, test__evolve__bookshelf_deleted;
  "test__evolve__bookshelf_renamed", `Quick, test__evolve__bookshelf_renamed;
  "test__evolve__book_added", `Quick, test__evolve__book_added;
  "test__evolve__book_removed", `Quick, test__evolve__book_removed;
]