module Isbn : sig
  type t [@@deriving eq, show]

  val create : string -> t option
end = struct
  type t = string [@@deriving eq, show]

  let string_to_digit_list integer_list =
    let open Containers in
    (String.fold_right (fun c agg -> (Char.to_int c - 48) :: agg) integer_list)
      []

  let check_range_values = List.for_all (fun x -> x < 10 && x >= 0)

  let check_isbn_sum isbn_list =
    let agg =
      List.mapi
        (fun i digit -> if i mod 2 = 0 then digit else 3 * digit)
        isbn_list
      |> List.fold_left ( + ) 0
    in
    agg mod 10 = 0

  let check_isbn isbn =
    let digit_list = isbn |> string_to_digit_list in
    check_range_values digit_list && check_isbn_sum digit_list

  let create isbn =
    if String.length isbn = 13 then
      match check_isbn isbn with false -> None | true -> Some isbn
    else None
end

module Pages : sig
  type t [@@deriving eq, show]

  val create : int -> t option
end = struct
  type t = int [@@deriving eq, show]

  let create page = if page > 0 then Some page else None
end

type book_id = Book_id of string [@@deriving eq, show]
type owner_id = Owner_id of string [@@deriving eq, show]
type bookshelf_id = Bookshelf_id of string [@@deriving eq, show]
