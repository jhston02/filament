open Filament

let book_events = Alcotest.testable Book.pp_e Book.equal_e
let book = Alcotest.testable Book.pp Book.equal

let create_test_values =
  let open Common in
  let book_id = Book_id "id" in
  let owner_id = Owner_id "OId" in
  let isbn = Isbn.create "9780441172719" |> Option.get in
  let pages = Pages.create 15 |> Option.get in
  (book_id, owner_id, isbn, pages)

let create_test_book =
  let book_id, owner_id, isbn, pages = create_test_values in
  Book.create book_id owner_id isbn pages

let test_book_created () =
  let book_id, owner_id, isbn, pages = create_test_values in
  Alcotest.(check (result (list book_events) string))
    "BookCreated is same" create_test_book
    (Ok [ Book.Private.create_book_created_event book_id owner_id isbn pages ])

let create_event_tests states valid f message =
  let open Alcotest in
  List.map
    (fun x -> check (result (list book_events) string) message valid (f x))
    states

let test_book_deleted () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let valid_book_states =
    [
      create_finished_book b;
      create_quit_book b;
      create_wanted_book b;
      create_reading_book b (Option.get (Common.Pages.create 20));
    ]
  in
  create_event_tests valid_book_states
    (Ok [ create_book_deleted_event book_id owner_id ])
    (fun x -> Book.delete x)
    "BookDeleted is same"

let test_book_delete_invalid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let invalid_book_state = create_deleted_book b in
  Alcotest.(check (result (list book_events) string))
    "BookDeleted is same" (Error "Already Deleted")
    (Book.delete invalid_book_state)

let test_book_finished () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let valid_book_states =
    [
      create_deleted_book b;
      create_wanted_book b;
      create_reading_book b (Option.get (Common.Pages.create 20));
    ]
  in
  create_event_tests valid_book_states
    (Ok [ create_book_finished_event book_id owner_id ])
    (fun x -> Book.finish_reading x)
    "BookFinished is same"

let test_book_finished_already_finished () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let invalid_book_state = create_deleted_book b in
  Alcotest.(check (result (list book_events) string))
    "BookFinished is same" (Error "Already finished book")
    (Book.finish_reading invalid_book_state)

let test_book_finished_already_quit () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let invalid_book_state = create_quit_book b in
  Alcotest.(check (result (list book_events) string))
    "BookFinished is same" (Error "The book was not finished")
    (Book.finish_reading invalid_book_state)
