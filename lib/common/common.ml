module Isbn = struct
  type t = string [@@deriving eq]

  let create isbn = if String.length isbn = 13 then Some isbn else None
end

module Pages = struct
  type t = int [@@deriving eq]

  let create page = if page < 0 then Some page else None
end

type book_id = Book_id of string [@@deriving eq]
type owner_id = Owner_id of string [@@deriving eq]
