module Log

open System

type [<Struct>] LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
let private value = function
    | ERROR -> 1
    | WARN  -> 2
    | INFO  -> 3
    | DEBUG -> 4
    | TRACE -> 5
let private pretty = function
    | ERROR -> "[ERR]"
    | WARN  -> "[WRN]"
    | INFO  -> "[INF]"
    | DEBUG -> "[DBG]"
    | TRACE -> "[TRC]"

let mutable private logLevel = None
let setLogLevel lvl = logLevel <- lvl

let mutable Logger = 
    fun lvl (x:string) -> 
        sprintf "[%s] %s : %s" (System.DateTime.UtcNow.ToString("O")) (pretty lvl) x |> Console.WriteLine

let internal log lvl = 
    fun fmt -> 
        let logger = 
            let logLvl = logLevel |> Option.map value |> Option.defaultValue 0
            let v = value lvl
            if v > logLvl  then ignore else Logger lvl
        Printf.ksprintf logger fmt
