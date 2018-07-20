[<AutoOpen>]
module Core

let (|InnerEx|_|) (ex: exn) =
    let rec inner : exn -> _= function
        | :? System.AggregateException as agg ->
            agg.InnerExceptions |> Seq.tryPick inner
        | :? 'T as iex -> Some iex
        | _ -> None
    inner ex 

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
    module Option = 
        let toAsync = function Some x -> x |> map Some | None -> ret None
        let bind f x = x |> bind (function Some x -> f x | None -> ret None)
    module Seq = 
        let choose f = bind (Seq.fold (fun s x -> s |> bind (fun s' -> f x |> map (function Some x' -> seq { yield! s'; yield x' } | None -> s') ) ) (ret Seq.empty))
    module Map = 
        let toAsync x = x |> Map.fold (fun s k v -> s |> bind (fun s' -> v |> map (fun v' -> Map.add k v' s'))) (ret Map.empty)