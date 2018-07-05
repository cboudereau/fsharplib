module Parsing

open System.Text.RegularExpressions

let (|Regex|_|) pattern source = 
    let m = Regex.Match(source, pattern, RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase)
    if m.Success then [ for x in m.Groups -> x.Value ] |> List.tail |> Some
    else None
