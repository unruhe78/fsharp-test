module Quicksort

let rec quicksort list =
    match list with
        | [] -> []
        | first::rest ->
            let smaller = rest |> List.filter (fun e -> e <  first) |> quicksort
            let larger  = rest |> List.filter (fun e -> e >= first) |> quicksort
            List.concat [smaller; [first]; larger]

