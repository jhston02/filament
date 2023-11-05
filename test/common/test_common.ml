open Filament.Common
open QCheck2.Gen

let negative_pages =
  QCheck2.Test.make ~count:1000 ~name:"pages_negative_returns_none"
    (-10000 -- -1) (fun l -> Pages.create l = None)

let positive_pages =
  QCheck2.Test.make ~count:1000 ~name:"pages_positive_returns_some" (1 -- 10000)
    (fun l -> Option.is_some (Pages.create l))

(* Function to generate ISBN-13 digits excluding the checksum *)
let gen_isbn13_digits = int_range 0 9 |> list_repeat 12

(* Calculate the checksum digit for ISBN-13 *)
let calculate_checksum_digit (digits : int list) : int =
  let weighted_sum =
    List.mapi (fun i digit -> if i mod 2 = 0 then digit else 3 * digit) digits
    |> List.fold_left ( + ) 0
  in
  let checksum = 10 - (weighted_sum mod 10) in
  if checksum = 10 then 0 else checksum

(* Generator for ISBN-13 numbers *)
let gen_isbn13 =
  let digits_gen = gen_isbn13_digits in
  digits_gen
  |> map (fun digits ->
         let checksum_digit = calculate_checksum_digit digits in
         let digits_str =
           List.map string_of_int (digits @ [ checksum_digit ])
         in
         String.concat "" digits_str)

let valid_isbn =
  QCheck2.Test.make ~count:1000 ~name:"valid_isbn_returns_some" gen_isbn13
    (fun isbn -> Option.is_some (Isbn.create isbn))

let () =
  let suite =
    List.map
      (QCheck_alcotest.to_alcotest ~verbose:true)
      [ positive_pages; negative_pages; valid_isbn ]
  in
  Alcotest.run "my test" [ ("suite", suite) ]
