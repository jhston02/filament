module Isbn : sig
  type t [@@deriving eq]

  val create : string -> t option
end = struct
  type t = string [@@deriving eq]

  let integer_to_digit_list integer =
    let rec integer_to_digit_list_impl integer agg =
      if integer < 10 then (integer, agg)
      else integer_to_digit_list_impl (integer / 10) ((integer mod 10) :: agg)
    in
    integer_to_digit_list_impl integer []

  let check_isbn_sum isbn_list =
    let check, list = isbn_list in
    let agg, _ =
      List.fold_left
        (fun (agg, multiple) value ->
          let agg = agg + (value * multiple) in
          let multiple = if multiple = 1 then 3 else 1 in
          (agg, multiple))
        (0, 1) list
    in
    let check_value = 10 - (agg mod 10) in
    if check_value = 10 then check = 0 else check = check_value

  let check_isbn isbn =
    let open Containers.Option.Infix in
    int_of_string_opt isbn >|= integer_to_digit_list >|= check_isbn_sum

  let create isbn =
    if String.length isbn = 13 then
      match check_isbn isbn with
      | Some false -> None
      | Some true -> Some isbn
      | None -> None
    else None
end

module Pages : sig
  type t [@@deriving eq]

  val create : int -> t option
end = struct
  type t = int [@@deriving eq]

  let create page = if page < 0 then Some page else None
end

type book_id = Book_id of string [@@deriving eq]
type owner_id = Owner_id of string [@@deriving eq]
