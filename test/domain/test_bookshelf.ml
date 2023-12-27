open Filament
open Filament.Common

let bookshelf_events = Alcotest.testable Bookshelf.pp_e Bookshelf.equal_e
let bookshelf = Alcotest.testable Bookshelf.pp Bookshelf.equal

let test__create__valid () =
  let open Common in
  let id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let name = "My Bookshelf" in
  let expected = Ok [(Bookshelf.Private.create_bookshelf_created_event id owner_id name)] in
  let result = Bookshelf.create id owner_id name in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookshelfCreated is same" expected result

let test__delete__valid () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let base = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" false in 
  let expected = Ok [Bookshelf.Private.create_bookshelf_deleted_event  bookshelf_id owner_id] in
  let result = Bookshelf.delete base in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookshelfDeleted is same" expected result

let test__delete__already_deleted__invalid () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let base = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" true in 
  let expected = Error "Bookshelf already deleted" in
  let result = Bookshelf.delete base in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookshelfDeleted is same" expected result

let test__rename__valid () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let base = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" false in 
  let name = "New Bookshelf Name" in
  let expected = Ok [Bookshelf.Private.create_bookshelf_renamed_event bookshelf_id owner_id "test" name] in
  let result = Bookshelf.rename base name in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookshelfRenamed is same" expected result

let test__rename__already_deleted__invalid () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let base = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" true in 
  let name = "New Bookshelf Name" in
  let expected = Error "Bookshelf already deleted" in
  let result = Bookshelf.rename base name in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookshelfRenamed is same" expected result

let test__add_book__valid () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let base = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" false in 
  let book_id = Book_id "book_id" in
  let expected = Ok [Bookshelf.Private.create_book_added_event bookshelf_id owner_id book_id] in
  let result = Bookshelf.add_book base book_id in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookAdded is same" expected result

let test__add_book__already_deleted__invalid () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let base = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" true in 
  let book_id = Book_id "book_id" in
  let expected = Error "Bookshelf already deleted" in
  let result = Bookshelf.add_book base book_id in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookAdded is same" expected result

let test__add_book__already_in_bookshelf__invalid () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let book_id = Book_id "book_id" in
  let base = Bookshelf.Private.create_bookshelf_with_book bookshelf_id owner_id "test" book_id in 
  let expected = Error "Book already in bookshelf" in
  let result = Bookshelf.add_book base book_id in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookAdded is same" expected result

let test__remove_book__valid () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let book_id = Book_id "book_id" in
  let base = Bookshelf.Private.create_bookshelf_with_book bookshelf_id owner_id "test" book_id in 
  let expected = Ok [Bookshelf.Private.create_book_removed_event bookshelf_id owner_id book_id] in
  let result = Bookshelf.remove_book base book_id in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookRemoved is same" expected result

let test__remove_book__already_deleted__invalid () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let book_id = Book_id "book_id" in
  let base = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" true in 
  let expected = Error "Bookshelf already deleted" in
  let result = Bookshelf.remove_book base book_id in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookRemoved is same" expected result

let test__remove_book__not_in_bookshelf__invalid () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let book_id = Book_id "book_id" in
  let base = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" false in 
  let expected = Error "Book not in bookshelf" in
  let result = Bookshelf.remove_book base book_id in
  Alcotest.(check (result (list bookshelf_events) string))
    "BookRemoved is same" expected result

let test__evolve__bookshelf_created () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let event = Bookshelf.Private.create_bookshelf_created_event bookshelf_id owner_id "test" in
  let expected = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" false in
  let result = Bookshelf.evolve Bookshelf.empty [event] in
  Alcotest.(check bookshelf) "Bookshelf is same" expected result

let test__evolve__bookshelf_deleted () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let event = Bookshelf.Private.create_bookshelf_deleted_event bookshelf_id owner_id in
  let base = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" false in
  let expected = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" true in
  let result = Bookshelf.evolve base [event] in
  Alcotest.(check bookshelf) "Bookshelf is same" expected result

let test__evolve__bookshelf_renamed () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let event = Bookshelf.Private.create_bookshelf_renamed_event bookshelf_id owner_id "test" "test1" in
  let base = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" false in
  let expected = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test1" false in
  let result = Bookshelf.evolve base [event] in
  Alcotest.(check bookshelf) "Bookshelf is same" expected result

let test__evolve__book_added () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let book_id = Book_id "Id" in
  let event = Bookshelf.Private.create_book_added_event bookshelf_id owner_id book_id in
  let base = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" false in
  let expected = Bookshelf.Private.create_bookshelf_with_book bookshelf_id owner_id "test" book_id in
  let result = Bookshelf.evolve base [event] in
  Alcotest.(check bookshelf) "Bookshelf is same" expected result

let test__evolve__book_removed () =
  let bookshelf_id = Bookshelf_id "id" in
  let owner_id = Owner_id "OId" in
  let book_id = Book_id "Id" in
  let event = Bookshelf.Private.create_book_removed_event bookshelf_id owner_id book_id in
  let base = Bookshelf.Private.create_bookshelf_with_book bookshelf_id owner_id "test" book_id in
  let expected = Bookshelf.Private.create_bookshelf bookshelf_id owner_id "test" false in
  let result = Bookshelf.evolve base [event] in
  Alcotest.(check bookshelf) "Bookshelf is same" expected result

let tests = 
  let open Alcotest in
  ( "Bookshelf tests", [
 test_case "test__create__valid" `Quick test__create__valid;
 test_case "test__delete__valid" `Quick test__delete__valid;
 test_case "test__delete__already_deleted__invalid" `Quick test__delete__already_deleted__invalid;
 test_case "test__rename__valid" `Quick test__rename__valid;
 test_case "test__rename__already_deleted__invalid" `Quick test__rename__already_deleted__invalid;
 test_case "test__add_book__valid" `Quick test__add_book__valid;
 test_case "test__add_book__already_deleted__invalid" `Quick test__add_book__already_deleted__invalid;
 test_case "test__add_book__already_in_bookshelf__invalid" `Quick test__add_book__already_in_bookshelf__invalid;
 test_case "test__remove_book__valid" `Quick test__remove_book__valid;
 test_case "test__remove_book__already_deleted__invalid" `Quick test__remove_book__already_deleted__invalid;
 test_case "test__remove_book__not_in_bookshelf__invalid" `Quick test__remove_book__not_in_bookshelf__invalid;
 test_case "test__evolve__bookshelf_created" `Quick test__evolve__bookshelf_created;
 test_case "test__evolve__bookshelf_deleted" `Quick test__evolve__bookshelf_deleted;
 test_case "test__evolve__bookshelf_renamed" `Quick test__evolve__bookshelf_renamed;
 test_case "test__evolve__book_added" `Quick test__evolve__book_added;
 test_case "test__evolve__book_removed" `Quick test__evolve__book_removed;
])

let get_tests () = [tests]
