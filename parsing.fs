module Parsing

open System.Text.RegularExpressions

let (|Int|_|) x = 
    match System.Int32.TryParse(x) with
    | true, i -> Some i
    | _ -> None

let (|Regex|_|) pattern source = 
    let m = Regex.Match(source, pattern, RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
    if m.Success then [ for x in m.Groups -> x.Value ] |> List.tail |> Some
    else None
