open Filament.Common

let negative =
  QCheck.Test.make ~count:1000 ~name:"pages_negative_returns_none"
    QCheck.(neg_int)
    (fun l -> Pages.create l = None)

let positive =
  QCheck.Test.make ~count:1000 ~name:"pages_positive_returns_some"
    QCheck.(pos_int)
    (fun l -> Option.is_some (Pages.create l))
