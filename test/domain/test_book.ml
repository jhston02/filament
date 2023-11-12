open Filament

let book_events = Alcotest.testable Book.pp_e Book.equal_e
let book = Alcotest.testable Book.pp Book.equal
