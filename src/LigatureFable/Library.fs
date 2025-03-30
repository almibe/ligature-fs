open Wander.Main
open Wander.Interpreter
open Ligature.Model
open Wander.Model
open System.Collections.Generic
open Wander.Library
open Fable.Core.JsInterop
open Wander.InMemoryStore

let runWithFns (fns: Dictionary<string, Any array -> Result<Any, LigatureError>>) (script: string) =
    let mutable resFns = stdFns (new InMemoryStore())
    for entry in fns do
        resFns <- Map.add (Term entry.Key) (Fn({doc = ""; examples = []; args = ""; result = ""}, 
            fun _ _ _ args -> 
                entry.Value (List.toArray args))) resFns
    run resFns Map.empty Map.empty script

let run = runWithDefaults

let printResult = printResult

let printAny = printAny

let ok value = Ok value

let error value = Error value

let networkToJs (network : Network) =
    let network = 
        Set.map
            (fun (Term e, Term a, v) ->
                let element = createEmpty
                element?``type`` <- "term"
                element?value <- e

                let role = createEmpty
                role?``type`` <- "term"
                role?value <- a

                let value = createEmpty
                value?``type`` <- 
                    match v with
                    | Value.Term _ -> "term"
                    | Value.Literal _ -> "literal"
                value?value <-
                    match v with
                    | Value.Term (Term t) -> t
                    | Value.Literal (Literal l) -> l
                    | _ -> failwith "TODO"
                [| element; role; value |])
            network
    let network = Array.ofSeq network
    let obj = createEmpty
    obj?``type`` <- "network"
    obj?value <- network
    obj

let rec recordToJs (record: Record) =
    let record =
        Seq.fold
            (fun state (key, value) ->
                match key with
                | Any.Term (Term t) ->
                    emitJsExpr (t, anyToJs value) "state[$0] = $1"
                    state
                | _ -> failwith "Unsupported record key.")
            (emitJsExpr () "{}")
            (Map.toSeq record)
    let obj = createEmpty
    obj?``type`` <- "record"
    obj?value <- record
    obj

and setToJs (set: AnySet) =
    let value = 
        Array.map (fun value ->
            anyToJs value)
            (Set.toArray set)
    let obj = createEmpty
    obj?``type`` <- "set"
    obj?value <- value
    obj

and anyToJs (any: Any) =
    match any with
    | Any.Term (Term t) -> 
        let obj = createEmpty
        obj?``type`` <- "term"
        obj?value <- t
        obj
    | Any.Literal (Literal l) -> 
        let obj = createEmpty
        obj?``type`` <- "literal"
        obj?value <- l
        obj
    | Any.Network n -> networkToJs n
    | Any.Tuple t ->
        let res = 
            List.map (fun any -> anyToJs any) t
            |> List.toArray
        let obj = createEmpty
        obj?``type`` <- "tuple"
        obj?value <- res
        obj
    | Any.Record record -> recordToJs record
    | Any.AnySet set -> setToJs set
    | x -> failwith $"Invalid call to anyToJs: {x}"

let resultToJs (res: Result<Any, LigatureError>) =
    match res with
    | Error err -> 
        let obj = createEmpty
        obj?``type`` <- "error"
        obj?value <- err
        obj
    | Ok any -> anyToJs any

let appendCanvas element (value: Result<Any, LigatureError>) =
    match value with
    | Ok (Any.Record value) ->
        let canvas = emitJsExpr () "document.createElement('canvas')"
        let ctx = emitJsExpr () "canvas.getContext('2d')"

        match value.TryFind(Any.Term(Term "height")) with
        | Some (Any.Term (Term height)) -> emitJsStatement (height) "canvas.height = $0"
        | _ -> ()

        match value.TryFind(Any.Term(Term "width")) with
        | Some (Any.Term (Term width)) -> emitJsStatement (width) "canvas.width = $0"
        | _ -> ()

        match value.TryFind(Any.Term(Term "instructions")) with
        | Some (Any.Tuple instructions) -> 
                List.iter (fun instruction -> 
                    match instruction with
                    | Any.Tuple [ Any.Term (Term "fill-style"); Any.Term (Term value) ] ->
                        emitJsStatement (value) "ctx.fillStyle = $0"
                    | Any.Tuple [ Any.Term (Term "fill-rect"); Any.Term (Term x); Any.Term (Term y); Any.Term (Term w); Any.Term (Term h)] ->
                        emitJsStatement (x, y, w, h) "ctx.fillRect($0, $1, $2, $3)"
                    | x -> failwith $"Invalid instruction: {x}.") instructions
        | _ -> failwith "Instructions required when drawing canvas."
        emitJsStatement () "element.appendChild(canvas)"
    | Ok value -> failwith $"Unexpected value passed to appendCanvas {value}"
    | Error err -> failwith err.UserMessage
