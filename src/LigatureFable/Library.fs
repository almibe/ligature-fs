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
        resFns <-
            Map.add
                (Term entry.Key)
                (Fn(
                    { doc = ""
                      examples = []
                      args = ""
                      result = "" },
                    fun _ _ _ args -> entry.Value(List.toArray args)
                ))
                resFns

    run resFns Map.empty Map.empty script

let run = runWithDefaults

let printResult = printResult

let printAny = printAny

let ok value = Ok value

let error value = Error value

let networkToJs (network: Network) =
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
                    | Value.Term(Term t) -> t
                    | Value.Literal(Literal l) -> l
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
                | Any.Term(Term t) ->
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
    let value = Array.map (fun value -> anyToJs value) (Set.toArray set)
    let obj = createEmpty
    obj?``type`` <- "set"
    obj?value <- value
    obj

and anyToJs (any: Any) =
    match any with
    | Any.Term(Term t) ->
        let obj = createEmpty
        obj?``type`` <- "term"
        obj?value <- t
        obj
    | Any.Literal(Literal l) ->
        let obj = createEmpty
        obj?``type`` <- "literal"
        obj?value <- l
        obj
    | Any.Network n -> networkToJs n
    | Any.Tuple t ->
        let res = List.map (fun any -> anyToJs any) t |> List.toArray
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
    | Ok(Any.Record value) ->
        let canvas = emitJsExpr () "document.createElement('canvas')"
        let ctx = emitJsExpr () "canvas.getContext('2d')"

        match value.TryFind(Any.Term(Term "height")) with
        | Some(Any.Term(Term height)) -> emitJsStatement (height) "canvas.height = $0"
        | _ -> ()

        match value.TryFind(Any.Term(Term "width")) with
        | Some(Any.Term(Term width)) -> emitJsStatement (width) "canvas.width = $0"
        | _ -> ()

        match value.TryFind(Any.Term(Term "instructions")) with
        | Some(Any.Tuple instructions) ->
            List.iter
                (fun instruction ->
                    match instruction with
                    | Any.Tuple [ Any.Term(Term "clear-rect")
                                  Any.Term(Term x)
                                  Any.Term(Term y)
                                  Any.Term(Term w)
                                  Any.Term(Term h) ] -> emitJsStatement (x, y, w, h) "ctx.clearRect($0, $1, $2, $3)"
                    | Any.Tuple [ Any.Term(Term "fill-rect")
                                  Any.Term(Term x)
                                  Any.Term(Term y)
                                  Any.Term(Term w)
                                  Any.Term(Term h) ] -> emitJsStatement (x, y, w, h) "ctx.fillRect($0, $1, $2, $3)"
                    | Any.Tuple [ Any.Term(Term "stroke-rect")
                                  Any.Term(Term x)
                                  Any.Term(Term y)
                                  Any.Term(Term w)
                                  Any.Term(Term h) ] -> emitJsStatement (x, y, w, h) "ctx.strokeRect($0, $1, $2, $3)"

                    | Any.Tuple [ Any.Term(Term "fill-text")
                                  Any.Literal(Literal text)
                                  Any.Term(Term x)
                                  Any.Term(Term y) ] -> emitJsStatement (text, x, y) "ctx.fillText($0, $1, $2)"
                    | Any.Tuple [ Any.Term(Term "stroke-text")
                                  Any.Literal(Literal text)
                                  Any.Term(Term x)
                                  Any.Term(Term y) ] -> emitJsStatement (text, x, y) "ctx.strokeText($0, $1, $2)"

                    | Any.Tuple [ Any.Term(Term "line-width"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.lineWidth($0)"
                    | Any.Tuple [ Any.Term(Term "line-cap"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.lineCap($0)"
                    | Any.Tuple [ Any.Term(Term "line-join"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.lineJoin($0)"
                    | Any.Tuple [ Any.Term(Term "miter-limit"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.miterLimit($0)"
                    //TODO line dash

                    | Any.Tuple [ Any.Term(Term "font"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.font($0)"
                    | Any.Tuple [ Any.Term(Term "font"); Any.Literal(Literal value) ] ->
                        emitJsStatement (value) "ctx.font($0)"
                    | Any.Tuple [ Any.Term(Term "text-align"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.textAlign($0)"
                    | Any.Tuple [ Any.Term(Term "text-baseline"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.textBaseline($0)"
                    | Any.Tuple [ Any.Term(Term "direction"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.direction($0)"
                    | Any.Tuple [ Any.Term(Term "letter-spacing"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.letterSpacing($0)"
                    | Any.Tuple [ Any.Term(Term "font-kerning"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.fontKerning($0)"
                    | Any.Tuple [ Any.Term(Term "font-stretch"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.fontStretch($0)"
                    | Any.Tuple [ Any.Term(Term "font-variant-caps"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.fontVariantCaps($0)"
                    | Any.Tuple [ Any.Term(Term "text-rendering"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.textRendering($0)"
                    | Any.Tuple [ Any.Term(Term "word-spacing"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.wordSpacing($0)"

                    | Any.Tuple [ Any.Term(Term "fill-style"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.fillStyle = $0"
                    | Any.Tuple [ Any.Term(Term "stroke-style"); Any.Term(Term value) ] ->
                        emitJsStatement (value) "ctx.strokeStyle = $0"

                    //TODO Gradients and patterns
                    //TODO Shadows

                    | Any.Tuple [ Any.Term(Term "begin-path") ] -> emitJsStatement () "ctx.beginPath()"
                    | Any.Tuple [ Any.Term(Term "close-path") ] -> emitJsStatement () "ctx.closePath()"
                    | Any.Tuple [ Any.Term(Term "move-to"); Any.Term(Term x); Any.Term(Term y) ] ->
                        emitJsStatement (x, y) "ctx.moveTo($0, $1)"
                    | Any.Tuple [ Any.Term(Term "line-to"); Any.Term(Term x); Any.Term(Term y) ] ->
                        emitJsStatement (x, y) "ctx.lineTo($0, $1)"
                    | Any.Tuple [ Any.Term(Term "bezier-curve-to")
                                  Any.Term(Term cp1x)
                                  Any.Term(Term cp1y)
                                  Any.Term(Term cp2x)
                                  Any.Term(Term cp2y)
                                  Any.Term(Term x)
                                  Any.Term(Term y) ] ->
                        emitJsStatement (cp1x, cp1y, cp2x, cp2y, x, y) "ctx.bezierCurveTo($0, $1, $2, $3, $4, $5)"
                    | Any.Tuple [ Any.Term(Term "quadratic-curve-to")
                                  Any.Term(Term cpx)
                                  Any.Term(Term cpy)
                                  Any.Term(Term x)
                                  Any.Term(Term y) ] ->
                        emitJsStatement (cpx, cpy, x, y) "ctx.bezierCurveTo($0, $1, $2, $3)"
                    //TODO arc
                    //TODO arc-to
                    //TODO ellipse
                    //TODO rect
                    //TODO round-rect

                    | Any.Tuple [ Any.Term(Term "fill") ] -> emitJsStatement () "ctx.fill()"
                    | Any.Tuple [ Any.Term(Term "stroke") ] -> emitJsStatement () "ctx.stroke()"


                    | x -> failwith $"Invalid instruction: {x}.")
                instructions
        | _ -> failwith "Instructions required when drawing canvas."

        emitJsStatement () "element.appendChild(canvas)"
    | Ok value -> failwith $"Unexpected value passed to appendCanvas {value}"
    | Error err -> failwith err.UserMessage

let simplify (network: Network): Set<string * (string * string)> =
    Set.map(fun (Term e,_,v) ->
        let value =
            match v with
            | Value.Literal (Literal l) -> "literal", l
            | Value.Term (Term t) -> "term", t
        e,value) network

let position (network: Set<string * (string * string)>): Map<string * string, float * float> =
    let rand = System.Random()
    Set.fold (fun state (e, (t, v)) -> 
        state
        |> Map.add ("term", e) (rand.NextDouble(), rand.NextDouble())
        |> Map.add (t, v) (rand.NextDouble(), rand.NextDouble())) Map.empty network

let computeForceDirectedLayout (network: Network) : Tuple =
    let network = simplify network
    let pos = position network

    Map.toSeq pos
    |> Seq.map (fun ((t, v), (x, y)) ->
        Any.Tuple [ 
            Any.Term(Term "fill-text")
            Any.Literal(Literal v)
            Any.Term(Term $"{x*400.0}")
            Any.Term(Term $"{y*400.0}") ])
    |> List.ofSeq

let drawNetwork element (value: Result<Any, LigatureError>) =
    match value with
    | Ok(Any.Record conf) ->
        match Map.tryFind (Any.Term(Term "network")) conf with
        | Some(Any.Network network) ->
            let instructions = computeForceDirectedLayout network
            appendCanvas element (Ok(Any.Record(Map.ofList [ 
                Any.Term(Term "width"), Any.Term(Term "400")
                Any.Term(Term "height"), Any.Term(Term "400")
                Any.Term(Term "instructions"), Any.Tuple instructions ])))
        | _ -> failwith "TODO"
    | r -> failwith $"Invalid agrument passed to drawNetwork: {printResult r}"
