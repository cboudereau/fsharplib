module String

open System

let icontains search source = (source:string).IndexOf((search:string), StringComparison.InvariantCultureIgnoreCase) <> -1
let (|Contains|_|) search source =
    if icontains search source then Some source
    else None 
