let () =
  Alcotest.run "Filament" (Test_common.get_tests () @ Test_book.get_tests ())
