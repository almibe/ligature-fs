open Wander.Main
open Wander.Interpreter
open Ligature.Model
open Wander.Model
open System.Collections.Generic
open Wander.Library
open Fable.Core
open Fable.Core.JsInterop
open Wander.InMemoryStore

let runWithFns (fns: Dictionary<string, Expression array -> Result<Expression, LigatureError>>) (script: string) =
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

let networkToJs (network: ABox) =
    let network =
        Set.map
            (fun value ->
                match value with
                | Assertion.Triple(Term e, Term a, v) ->
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
                        | Value.Literal { id = l } -> l //TODO add type and lang tag

                    [| element; role; value |]
                | Assertion.Instance(Term i, ConceptExpr.AtomicConcept(Term c)) ->
                    let element = createEmpty
                    element?``type`` <- "term"
                    element?value <- i

                    let role = createEmpty
                    role?``type`` <- "term"
                    role?value <- ":"

                    let value = createEmpty

                    value?``type`` <- "term"

                    value?value <- c

                    [| element; role; value |])
            network

    let network = Array.ofSeq network
    let obj = createEmpty
    obj?``type`` <- "network"
    obj?value <- network
    obj

let rec nodeToJs
    ({ name = Term name
       attributes = attributes
       children = children }: Node)
    =
    let attributes =
        Seq.fold
            (fun state (key, value) ->
                match key with
                | Term t ->
                    emitJsExpr (t, anyToJs value) "state[$0] = $1"
                    state
                | _ -> failwith "Unsupported attributes key.")
            (emitJsExpr () "{}")
            (Map.toSeq attributes)

    let children = List.map (fun value -> anyToJs value) children

    let obj = createEmpty
    obj?``type`` <- "node"
    obj?name <- name
    obj?attributes <- attributes
    obj?children <- Array.ofList children
    obj

and anyToJs (any: Expression) =
    match any with
    | Expression.Term(Term t) ->
        let obj = createEmpty
        obj?``type`` <- "term"
        obj?value <- t
        obj
    | Expression.Literal { value = l } -> //TODO add datatype and langtag
        let obj = createEmpty
        obj?``type`` <- "literal"
        obj?value <- l
        obj
    | Expression.ABox n -> networkToJs n
    | Expression.Tuple t ->
        let res = List.map (fun any -> anyToJs any) t |> List.toArray
        let obj = createEmpty
        obj?``type`` <- "tuple"
        obj?value <- res
        obj
    | Expression.NodeLiteral node -> nodeToJs node
    | x -> failwith $"Invalid call to anyToJs: {x}"

let resultToJs (res: Result<Expression, LigatureError>) =
    match res with
    | Error err ->
        let obj = createEmpty
        obj?``type`` <- "error"
        obj?value <- err
        obj
    | Ok any -> anyToJs any

let rec createElement
    { name = Term tag
      attributes = attributes
      children = children }
    =
    let newElement = emitJsExpr (tag) "document.createElement($0)"

    Map.iter
        (fun key value ->
            match key, value with
            | Term key, Expression.Literal { value = content } ->
                emitJsStatement (key, content) "newElement.setAttribute($0, $1)"
            | Term key, _ -> failwith $"Invalid attribute - {key}")
        attributes

    List.iter
        (fun value ->
            match value with
            | Expression.Literal { value = content } -> emitJsStatement (content) "newElement.append($0)"
            | Expression.NodeLiteral node ->
                let childElement = createElement node
                emitJsStatement (childElement) "newElement.append($0)"
            | x -> printfn $"ignoring value - {x}")
        children

    newElement

let appendHtml element (value: Result<Expression, LigatureError>) =
    match value with
    | Ok(Expression.NodeLiteral node) ->
        let newElement = createElement node
        emitJsStatement () "element.append(newElement)"
    | _ -> ()

let equivalent left right = ConceptExpr.Equivalent left, right

let concept name = ConceptExpr.AtomicConcept name

[<Emit("() => { if (typeof $0 === 'string') { return concept($0)} else { return $0 } }")>]
let handleConcept concept = jsNative

let ``and`` left right =
    ConceptExpr.And [ handleConcept left; handleConcept right ]

let not concept = ConceptExpr.Not(handleConcept concept)

let instance individual concept =
    Assertion.Instance individual, handleConcept concept

let tBox definitions : TBox = []

let aBox assertions : ABox = Set.empty

let isConsistent tBox aBox : bool =
    match Ligature.Interpreter.isConsistent tBox aBox with
    | Ok value -> value
    | Error err -> failwith err.UserMessage

let appendCanvas element (value: Result<Expression, LigatureError>) = failwith "TODO"
// match value with
// | Ok(Any.Node value) ->
//     let canvas = emitJsExpr () "document.createElement('canvas')"
//     let ctx = emitJsExpr () "canvas.getContext('2d')"

//     match value.TryFind(Any.Term(Term "height")) with
//     | Some(Any.Term(Term height)) -> emitJsStatement (height) "canvas.height = $0"
//     | _ -> ()

//     match value.TryFind(Any.Term(Term "width")) with
//     | Some(Any.Term(Term width)) -> emitJsStatement (width) "canvas.width = $0"
//     | _ -> ()

//     match value.TryFind(Any.Term(Term "instructions")) with
//     | Some(Any.Tuple instructions) ->
//         List.iter
//             (fun instruction ->
//                 match instruction with
//                 | Any.Tuple [ Any.Term(Term "clear-rect")
//                               Any.Term(Term x)
//                               Any.Term(Term y)
//                               Any.Term(Term w)
//                               Any.Term(Term h) ] -> emitJsStatement (x, y, w, h) "ctx.clearRect($0, $1, $2, $3)"
//                 | Any.Tuple [ Any.Term(Term "fill-rect")
//                               Any.Term(Term x)
//                               Any.Term(Term y)
//                               Any.Term(Term w)
//                               Any.Term(Term h) ] -> emitJsStatement (x, y, w, h) "ctx.fillRect($0, $1, $2, $3)"
//                 | Any.Tuple [ Any.Term(Term "stroke-rect")
//                               Any.Term(Term x)
//                               Any.Term(Term y)
//                               Any.Term(Term w)
//                               Any.Term(Term h) ] -> emitJsStatement (x, y, w, h) "ctx.strokeRect($0, $1, $2, $3)"

//                 | Any.Tuple [ Any.Term(Term "fill-text")
//                               Any.Literal { content = text
//                                             datatype = None
//                                             langTag = None }
//                               Any.Term(Term x)
//                               Any.Term(Term y) ] -> emitJsStatement (text, x, y) "ctx.fillText($0, $1, $2)"
//                 | Any.Tuple [ Any.Term(Term "stroke-text")
//                               Any.Literal { content = text
//                                             datatype = None
//                                             langTag = None }
//                               Any.Term(Term x)
//                               Any.Term(Term y) ] -> emitJsStatement (text, x, y) "ctx.strokeText($0, $1, $2)"

//                 | Any.Tuple [ Any.Term(Term "draw-line")
//                               Any.Term(Term x)
//                               Any.Term(Term y)
//                               Any.Term(Term x2)
//                               Any.Term(Term y2) ] ->
//                     emitJsStatement
//                         (x, y, x2, y2)
//                         "ctx.beginPath(); ctx.moveTo($0, $1); ctx.lineTo($2, $3); ctx.stroke()"

//                 | Any.Tuple [ Any.Term(Term "line-width"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.lineWidth($0)"
//                 | Any.Tuple [ Any.Term(Term "line-cap"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.lineCap($0)"
//                 | Any.Tuple [ Any.Term(Term "line-join"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.lineJoin($0)"
//                 | Any.Tuple [ Any.Term(Term "miter-limit"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.miterLimit($0)"
//                 //TODO line dash

//                 | Any.Tuple [ Any.Term(Term "font"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.font($0)"
//                 | Any.Tuple [ Any.Term(Term "font"); Any.Literal { content = value } ] ->
//                     emitJsStatement (value) "ctx.font($0)"
//                 | Any.Tuple [ Any.Term(Term "text-align"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.textAlign($0)"
//                 | Any.Tuple [ Any.Term(Term "text-baseline"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.textBaseline($0)"
//                 | Any.Tuple [ Any.Term(Term "direction"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.direction($0)"
//                 | Any.Tuple [ Any.Term(Term "letter-spacing"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.letterSpacing($0)"
//                 | Any.Tuple [ Any.Term(Term "font-kerning"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.fontKerning($0)"
//                 | Any.Tuple [ Any.Term(Term "font-stretch"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.fontStretch($0)"
//                 | Any.Tuple [ Any.Term(Term "font-variant-caps"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.fontVariantCaps($0)"
//                 | Any.Tuple [ Any.Term(Term "text-rendering"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.textRendering($0)"
//                 | Any.Tuple [ Any.Term(Term "word-spacing"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.wordSpacing($0)"

//                 | Any.Tuple [ Any.Term(Term "fill-style"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.fillStyle = $0"
//                 | Any.Tuple [ Any.Term(Term "stroke-style"); Any.Term(Term value) ] ->
//                     emitJsStatement (value) "ctx.strokeStyle = $0"

//                 //TODO Gradients and patterns
//                 //TODO Shadows

//                 | Any.Tuple [ Any.Term(Term "begin-path") ] -> emitJsStatement () "ctx.beginPath()"
//                 | Any.Tuple [ Any.Term(Term "close-path") ] -> emitJsStatement () "ctx.closePath()"
//                 | Any.Tuple [ Any.Term(Term "move-to"); Any.Term(Term x); Any.Term(Term y) ] ->
//                     emitJsStatement (x, y) "ctx.moveTo($0, $1)"
//                 | Any.Tuple [ Any.Term(Term "line-to"); Any.Term(Term x); Any.Term(Term y) ] ->
//                     emitJsStatement (x, y) "ctx.lineTo($0, $1)"
//                 | Any.Tuple [ Any.Term(Term "bezier-curve-to")
//                               Any.Term(Term cp1x)
//                               Any.Term(Term cp1y)
//                               Any.Term(Term cp2x)
//                               Any.Term(Term cp2y)
//                               Any.Term(Term x)
//                               Any.Term(Term y) ] ->
//                     emitJsStatement (cp1x, cp1y, cp2x, cp2y, x, y) "ctx.bezierCurveTo($0, $1, $2, $3, $4, $5)"
//                 | Any.Tuple [ Any.Term(Term "quadratic-curve-to")
//                               Any.Term(Term cpx)
//                               Any.Term(Term cpy)
//                               Any.Term(Term x)
//                               Any.Term(Term y) ] ->
//                     emitJsStatement (cpx, cpy, x, y) "ctx.bezierCurveTo($0, $1, $2, $3)"
//                 //TODO arc
//                 //TODO arc-to
//                 //TODO ellipse
//                 //TODO rect
//                 //TODO round-rect

//                 | Any.Tuple [ Any.Term(Term "fill") ] -> emitJsStatement () "ctx.fill()"
//                 | Any.Tuple [ Any.Term(Term "stroke") ] -> emitJsStatement () "ctx.stroke()"


//                 | x -> failwith $"Invalid instruction: {x}.")
//             instructions
//     | _ -> failwith "Instructions required when drawing canvas."

//     emitJsStatement () "element.appendChild(canvas)"
// | Ok value -> failwith $"Unexpected value passed to appendCanvas {value}"
// | Error err -> failwith err.UserMessage
