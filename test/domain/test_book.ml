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
  let _ = create_event_tests valid_book_states
    (Ok [ create_book_deleted_event book_id owner_id ])
    (fun x -> Book.delete x)
    "BookDeleted is same" in 
  () 
  

let test_book_delete_invalid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let invalid_book_state = create_deleted_book b in
  Alcotest.(check (result (list book_events) string))
    "BookDeleted is same" (Error "Already deleted")
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
  let _ = create_event_tests valid_book_states
    (Ok [ create_book_finished_event book_id owner_id ])
    (fun x -> Book.finish_reading x)
    "BookFinished is same" in
  ()

let test_book_finished_already_finished () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let invalid_book_state = create_finished_book b in
  Alcotest.(check (result (list book_events) string))
    "BookFinished is same" (Error "Already finished book")
    (Book.finish_reading invalid_book_state)

let test_book_finished_already_quit () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let invalid_book_state = create_quit_book b in
  Alcotest.(check (result (list book_events) string))
    "BookFinished is same" (Error "This book was not finished")
    (Book.finish_reading invalid_book_state)


let test_book_wanted () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let valid_book_states =
    [
      create_deleted_book b;
      create_wanted_book b;
      create_reading_book b (Option.get (Common.Pages.create 20));
      create_quit_book b;
      create_finished_book b
    ]
  in
  let _ = create_event_tests valid_book_states
    (Ok [ create_book_wanted_event book_id owner_id ])
    (fun x -> Book.mark_as_wanted x)
    "BookWanted is same" in
  ()

let test_book_started_reading () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let valid_book_states =
    [
      create_deleted_book b;
      create_wanted_book b;
      create_quit_book b;
      create_finished_book b
    ]
  in
  let _ = create_event_tests valid_book_states
    (Ok [ create_book_started_event book_id owner_id ])
    (fun x -> Book.start_reading x)
    "BookWanted is same" in
  ()


let test_book_reading_already_reading () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let invalid_book_state = create_reading_book b pages in
  Alcotest.(check (result (list book_events) string))
    "BookReading is same" (Error "Already reading book")
    (Book.start_reading invalid_book_state)

let test_read_to_page_already_reading () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let source_page = Common.Pages.create 1 |> Option.get in
  let end_page = Common.Pages.create 10 |> Option.get in
  let valid_book_state = create_reading_book b source_page in
  let r = create_read_to_page_event book_id owner_id source_page end_page in
  Alcotest.(check (result (list book_events) string)) "ReadToPage is same" (Ok [r]) (Book.read_to_page valid_book_state end_page)

let test_read_to_page_not_started () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let source_page = Common.Pages.create 1 |> Option.get in
  let end_page = Common.Pages.create 10 |> Option.get in
  let valid_book_state = create_wanted_book b in
  let r = Ok[create_book_started_event book_id owner_id ; create_read_to_page_event book_id owner_id source_page end_page] in
  Alcotest.(check (result (list book_events) string)) "ReadToPage is same" r (Book.read_to_page valid_book_state end_page)

let test_read_to_page_not_finished () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let source_page = Common.Pages.create 10 |> Option.get in
  let end_page = Common.Pages.create 15 |> Option.get in
  let valid_book_state = create_reading_book b source_page in
  let r = Ok[create_read_to_page_event book_id owner_id source_page end_page ; create_book_finished_event book_id owner_id] in
  Alcotest.(check (result (list book_events) string)) "ReadToPage is same" r (Book.read_to_page valid_book_state end_page)

let test_read_to_page_not_started_not_finished () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let source_page = Common.Pages.create 1 |> Option.get in
  let end_page = Common.Pages.create 15 |> Option.get in
  let valid_book_state = create_wanted_book b in
  let r = Ok[create_book_started_event book_id owner_id ; create_read_to_page_event book_id owner_id source_page end_page ; create_book_finished_event book_id owner_id] in
  Alcotest.(check (result (list book_events) string)) "ReadToPage is same" r (Book.read_to_page valid_book_state end_page)

let test_read_to_page_read_backwards () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let source_page = Common.Pages.create 10 |> Option.get in
  let end_page = Common.Pages.create 9 |> Option.get in
  let valid_book_state = create_reading_book b source_page in
  let r = Error "Enter valid page number" in
  Alcotest.(check (result (list book_events) string)) "ReadToPage is same" r (Book.read_to_page valid_book_state end_page)

let test_read_to_page_past_end_of_book () = 
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let source_page = Common.Pages.create 10 |> Option.get in
  let end_page = Common.Pages.create 16 |> Option.get in
  let valid_book_state = create_reading_book b source_page in
  let r = Error "Enter valid page number" in
  Alcotest.(check (result (list book_events) string)) "ReadToPage is same" r (Book.read_to_page valid_book_state end_page)


let event_tests =
  let open Alcotest in
  ( "Event unit",
    [
      test_case "Book created event ok" `Quick test_book_created;
      test_case "Test double deleted book" `Quick test_book_delete_invalid;
      test_case "Test double finish book" `Quick test_book_finished_already_finished;
      test_case "Test finished already quit book" `Quick test_book_finished_already_quit;
      test_case "Test double reading book" `Quick test_book_reading_already_reading;
      test_case "Test read to page already reading" `Quick test_read_to_page_already_reading;
      test_case "Test read to page not finished" `Quick test_read_to_page_not_finished;
      test_case "Test read to page not started" `Quick test_read_to_page_not_started;
      test_case "Test read to page not started and not finished" `Quick test_read_to_page_not_started_not_finished;
      test_case "Test read to page past end of book" `Quick test_read_to_page_past_end_of_book;
      test_case "Test read to page backwards" `Quick test_read_to_page_read_backwards;
      test_case "Test finish book" `Quick test_book_finished;
      test_case "Test want book" `Quick test_book_wanted;
      test_case "Test delete book" `Quick test_book_deleted;
      test_case "Test start reading book" `Quick test_book_started_reading
      ] )

let get_tests () = 
  [event_tests]