module Core

type [<Struct>] NonEmptyList<'a> = private NonEmptyList of 'a list

module NonEmptyList = 
    let build l = if l |> List.isEmpty then None else NonEmptyList l |> Some
    let singleton x = NonEmptyList [x]
    let map f (NonEmptyList l) = l |> List.map f |> NonEmptyList
    let append (NonEmptyList x) (NonEmptyList y) = List.append x y |> NonEmptyList
    let combine x y = Option.map2 append x y |> Option.orElse x |> Option.orElse y
    let value (NonEmptyList l) = l

module Async = 
    let map f x = async { let! x' = x in return f x' }
    let bind f x =  async.Bind(x, f)
    let ret x = async.Return(x)
    let ofOption zero = function Some x -> x | None -> ret zero