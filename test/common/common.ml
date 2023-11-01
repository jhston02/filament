open Common

let failing =
  QCheck.Test.make ~count:1000 ~name:"pages_always_positive"
    QCheck.(neg_int)
    (fun l -> List.rev (List.rev l) = l)
