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

let test__events__created__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  Alcotest.(check (result (list book_events) string))
    "BookCreated is same" create_test_book
    (Ok [ Book.Private.create_book_created_event book_id owner_id isbn pages ])

let create_event_tests states valid f message =
  let open Alcotest in
  List.map
    (fun x -> check (result (list book_events) string) message valid (f x))
    states

let test__events__delete__valid () =
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
  let _ =
    create_event_tests valid_book_states
      (Ok [ create_book_deleted_event book_id owner_id ])
      (fun x -> Book.delete x)
      "BookDeleted is same"
  in
  ()

let test__events__delete__invalid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let invalid_book_state = create_deleted_book b in
  Alcotest.(check (result (list book_events) string))
    "BookDeleted is same" (Error "Already deleted")
    (Book.delete invalid_book_state)

let test__events__finish__valid () =
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
  let _ =
    create_event_tests valid_book_states
      (Ok [ create_book_finished_event book_id owner_id ])
      (fun x -> Book.finish_reading x)
      "BookFinished is same"
  in
  ()

let test__events__finish__already_finished__invalid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let invalid_book_state = create_finished_book b in
  Alcotest.(check (result (list book_events) string))
    "BookFinished is same" (Error "Already finished book")
    (Book.finish_reading invalid_book_state)

let test__events__finish__already_quit__invalid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let invalid_book_state = create_quit_book b in
  Alcotest.(check (result (list book_events) string))
    "BookFinished is same" (Error "This book was not finished")
    (Book.finish_reading invalid_book_state)

let test__events__wanted__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let valid_book_states =
    [
      create_deleted_book b;
      create_wanted_book b;
      create_reading_book b (Option.get (Common.Pages.create 20));
      create_quit_book b;
      create_finished_book b;
    ]
  in
  let _ =
    create_event_tests valid_book_states
      (Ok [ create_book_wanted_event book_id owner_id ])
      (fun x -> Book.mark_as_wanted x)
      "BookWanted is same"
  in
  ()

let test__events__started_reading__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let valid_book_states =
    [
      create_deleted_book b;
      create_wanted_book b;
      create_quit_book b;
      create_finished_book b;
    ]
  in
  let _ =
    create_event_tests valid_book_states
      (Ok [ create_book_started_event book_id owner_id ])
      (fun x -> Book.start_reading x)
      "BookWanted is same"
  in
  ()

let test__events__start_reading__already_reading__invalid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let invalid_book_state = create_reading_book b pages in
  Alcotest.(check (result (list book_events) string))
    "BookReading is same" (Error "Already reading book")
    (Book.start_reading invalid_book_state)

let test__events__read_to_page__already_reading__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let source_page = Common.Pages.create 1 |> Option.get in
  let end_page = Common.Pages.create 10 |> Option.get in
  let valid_book_state = create_reading_book b source_page in
  let r = create_read_to_page_event book_id owner_id source_page end_page in
  Alcotest.(check (result (list book_events) string))
    "ReadToPage is same" (Ok [ r ])
    (Book.read_to_page valid_book_state end_page)

let test__events__read_to_page__not_started__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let source_page = Common.Pages.create 1 |> Option.get in
  let end_page = Common.Pages.create 10 |> Option.get in
  let valid_book_state = create_wanted_book b in
  let r =
    Ok
      [
        create_book_started_event book_id owner_id;
        create_read_to_page_event book_id owner_id source_page end_page;
      ]
  in
  Alcotest.(check (result (list book_events) string))
    "ReadToPage is same" r
    (Book.read_to_page valid_book_state end_page)

let test__events__read_to_page__not_finished__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let source_page = Common.Pages.create 10 |> Option.get in
  let end_page = Common.Pages.create 15 |> Option.get in
  let valid_book_state = create_reading_book b source_page in
  let r =
    Ok
      [
        create_read_to_page_event book_id owner_id source_page end_page;
        create_book_finished_event book_id owner_id;
      ]
  in
  Alcotest.(check (result (list book_events) string))
    "ReadToPage is same" r
    (Book.read_to_page valid_book_state end_page)

let test__events__read_to_page__not_started_not_finished__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let source_page = Common.Pages.create 1 |> Option.get in
  let end_page = Common.Pages.create 15 |> Option.get in
  let valid_book_state = create_wanted_book b in
  let r =
    Ok
      [
        create_book_started_event book_id owner_id;
        create_read_to_page_event book_id owner_id source_page end_page;
        create_book_finished_event book_id owner_id;
      ]
  in
  Alcotest.(check (result (list book_events) string))
    "ReadToPage is same" r
    (Book.read_to_page valid_book_state end_page)

let test__events__read_to_page__read_backwards__invalid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let source_page = Common.Pages.create 10 |> Option.get in
  let end_page = Common.Pages.create 9 |> Option.get in
  let valid_book_state = create_reading_book b source_page in
  let r = Error "Enter valid page number" in
  Alcotest.(check (result (list book_events) string))
    "ReadToPage is same" r
    (Book.read_to_page valid_book_state end_page)

let test__events__read_to_page__past_end_of_book__invalid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let open Book.Private in
  let b = { id = book_id; owner_id; isbn; total_pages = pages } in
  let source_page = Common.Pages.create 10 |> Option.get in
  let end_page = Common.Pages.create 16 |> Option.get in
  let valid_book_state = create_reading_book b source_page in
  let r = Error "Enter valid page number" in
  Alcotest.(check (result (list book_events) string))
    "ReadToPage is same" r
    (Book.read_to_page valid_book_state end_page)

let test__book__evolve__create__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let create_event =
    Book.Private.create_book_created_event book_id owner_id isbn pages
  in
  let r =
    Book.Private.create_wanted_book
      ({ id = book_id; owner_id; isbn; total_pages = pages }
        : Book.Private.p_book)
  in
  Alcotest.(check book)
    "Book is same" r
    (Book.evolve (Book.empty ()) [ create_event ])

let test__book__evolve__delete__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let delete_event = Book.Private.create_book_deleted_event book_id owner_id in
  let base_book =
    Book.Private.create_wanted_book
      ({ id = book_id; owner_id; isbn; total_pages = pages }
        : Book.Private.p_book)
  in
  let r =
    Book.Private.create_deleted_book
      ({ id = book_id; owner_id; isbn; total_pages = pages }
        : Book.Private.p_book)
  in
  Alcotest.(check book)
    "Book is same" r
    (Book.evolve base_book [ delete_event ])

let test__book__evolve__finish__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let finish_event = Book.Private.create_book_finished_event book_id owner_id in
  let base_book =
    Book.Private.create_wanted_book
      ({ id = book_id; owner_id; isbn; total_pages = pages }
        : Book.Private.p_book)
  in
  let r =
    Book.Private.create_finished_book
      ({ id = book_id; owner_id; isbn; total_pages = pages }
        : Book.Private.p_book)
  in
  Alcotest.(check book)
    "Book is same" r
    (Book.evolve base_book [ finish_event ])

let test__book__evolve__start__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let start_event = Book.Private.create_book_started_event book_id owner_id in
  let base_book =
    Book.Private.create_wanted_book
      ({ id = book_id; owner_id; isbn; total_pages = pages }
        : Book.Private.p_book)
  in
  let r =
    Book.Private.create_reading_book
      ({ id = book_id; owner_id; isbn; total_pages = pages }
        : Book.Private.p_book)
      (Common.Pages.create 1 |> Option.get)
  in
  Alcotest.(check book) "Book is same" r (Book.evolve base_book [ start_event ])

let test__book__evolve__want__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let want_event = Book.Private.create_book_wanted_event book_id owner_id in
  let base_book =
    Book.Private.create_wanted_book
      ({ id = book_id; owner_id; isbn; total_pages = pages }
        : Book.Private.p_book)
  in
  let r =
    Book.Private.create_wanted_book
      ({ id = book_id; owner_id; isbn; total_pages = pages }
        : Book.Private.p_book)
  in
  Alcotest.(check book) "Book is same" r (Book.evolve base_book [ want_event ])

let test__book__evolve__quit__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let quit_event = Book.Private.create_book_quit_event book_id owner_id in
  let base_book =
    Book.Private.create_wanted_book
      ({ id = book_id; owner_id; isbn; total_pages = pages }
        : Book.Private.p_book)
  in
  let r =
    Book.Private.create_quit_book
      ({ id = book_id; owner_id; isbn; total_pages = pages }
        : Book.Private.p_book)
  in
  Alcotest.(check book) "Book is same" r (Book.evolve base_book [ quit_event ])

let test__book__evolve__reading__valid () =
  let book_id, owner_id, isbn, pages = create_test_values in
  let read_event =
    Book.Private.create_read_to_page_event book_id owner_id
      (Common.Pages.create 1 |> Option.get)
      (Common.Pages.create 15 |> Option.get)
  in
  let base_book =
    Book.Private.create_wanted_book
      ({ id = book_id; owner_id; isbn; total_pages = pages }
        : Book.Private.p_book)
  in
  let r =
    Book.Private.create_reading_book
      ({ id = book_id; owner_id; isbn; total_pages = pages }
        : Book.Private.p_book)
      (Common.Pages.create 15 |> Option.get)
  in
  Alcotest.(check book) "Book is same" r (Book.evolve base_book [ read_event ])

(* These are the positive case tests aka happy path *)
let event_tests_valid =
  let open Alcotest in
  ( "Generate book events result Ok",
    [
      test_case "Book created event ok" `Quick test__events__created__valid;
      test_case "Test read to page already reading" `Quick
        test__events__read_to_page__already_reading__valid;
      test_case "Test read to page not finished" `Quick
        test__events__read_to_page__not_finished__valid;
      test_case "Test read to page not started" `Quick
        test__events__read_to_page__not_started__valid;
      test_case "Test read to page not started and not finished" `Quick
        test__events__read_to_page__not_started_not_finished__valid;
      test_case "Test finish book" `Quick test__events__finish__valid;
      test_case "Test want book" `Quick test__events__wanted__valid;
      test_case "Test delete book" `Quick test__events__delete__valid;
      test_case "Test start reading book" `Quick
        test__events__started_reading__valid;
    ] )

(* These are the negative case tests *)
let event_tests_invalid =
  let open Alcotest in
  ( "Generate book events error",
    [
      test_case "Test read to page backwards" `Quick
        test__events__read_to_page__read_backwards__invalid;
      test_case "Test read to page past end of book" `Quick
        test__events__read_to_page__past_end_of_book__invalid;
      test_case "Test double reading book" `Quick
        test__events__start_reading__already_reading__invalid;
      test_case "Test finished already quit book" `Quick
        test__events__finish__already_quit__invalid;
      test_case "Test double finish book" `Quick
        test__events__finish__already_finished__invalid;
      test_case "Test double deleted book" `Quick test__events__delete__invalid;
    ] )

let book_test_evolve =
  let open Alcotest in
  ( "Evolve books",
    [
      test_case "Test apply create event" `Quick
        test__book__evolve__create__valid;
      test_case "Test apply delete event" `Quick
        test__book__evolve__delete__valid;
      test_case "Test apply finish event" `Quick
        test__book__evolve__finish__valid;
      test_case "Test apply quit event" `Quick test__book__evolve__quit__valid;
      test_case "Test apply reading event" `Quick
        test__book__evolve__reading__valid;
      test_case "Test apply start event" `Quick test__book__evolve__start__valid;
      test_case "Test apply  want event" `Quick test__book__evolve__want__valid;
    ] )

let get_tests () = [ event_tests_valid; event_tests_invalid; book_test_evolve ]
