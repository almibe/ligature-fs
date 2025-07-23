open Wander.Main
open Ligature.InMemoryStore
open Ligature.Model
open Wander.Model
open System.Collections.Generic
open Wander.Library
open Fable.Core
open Fable.Core.JsInterop

let runWithFns (fns: Dictionary<string, Expression array -> Result<Expression, LigatureError>>) (script: string) =
    let mutable resFns = stdFns (new InMemoryStore())

    for entry in fns do
        resFns <-
            Map.add
                (Term entry.Key)
                (Fn.Fn(
                    { doc = ""
                      examples = []
                      args = ""
                      result = "" },
                    fun _ _ application -> entry.Value(List.toArray application.arguments)
                ))
                resFns

    run resFns Map.empty script

let runAndPrint = runWithDefaults >> printResult

let printResult = printResult

let printAny = printExpression

let ok value = Ok value

let error value = Error value

let aBoxToJs (aBox: Assertions) =
    let network =
        Set.map
            (fun value ->
                match value with
                | Assertion.Triple(individual, Term a, filler) ->
                    let element = createEmpty
                    element?``type`` <- "term"
                    element?value <- individual.value

                    let role = createEmpty
                    role?``type`` <- "term"
                    role?value <- a

                    let value = createEmpty

                    value?``type`` <- "term"
                    // match v with
                    // | Value.Term _ -> "term"
                    // | Value.Literal _ -> "literal"

                    value?value <- filler.value
                    // match v with
                    // | Value.Term(Term t) -> t
                    // | Value.Literal { id = l } -> l //TODO add type and lang tag

                    [| element; role; value |]
                | Assertion.Instance(individual, ConceptExpr.AtomicConcept(Term c)) ->
                    let element = createEmpty
                    element?``type`` <- "term"
                    element?value <- individual.value

                    let role = createEmpty
                    role?``type`` <- "term"
                    role?value <- ":"

                    let value = createEmpty

                    value?``type`` <- "term"

                    value?value <- c

                    [| element; role; value |])
            aBox

    let network = Array.ofSeq network
    let obj = createEmpty
    obj?``type`` <- "assertions"
    obj?value <- network
    obj

let rec createElement
    ({ root = root
       links = links
       concepts = concepts }: ObjectView)
    =
    failwith "TODO"
// let newElement = emitJsExpr tag "document.createElement($0)"

// Map.iter
//     (fun key value ->
//         match key, value with
//         | Term key, Expression.Element { value = content } ->
//             emitJsStatement (key, content) "newElement.setAttribute($0, $1)"
//         | Term key, _ -> failwith $"Invalid attribute - {key}")
//     attributes

// List.iter
//     (fun value ->
//         match value with
//         | Expression.Element { value = content } -> emitJsStatement content "newElement.append($0)"
//         | Expression.NodeLiteral node ->
//             let childElement = createElement node
//             emitJsStatement childElement "newElement.append($0)"
//         | Expression.Term(Term t) -> emitJsStatement t "newElement.append($0)"
//         | x -> failwith $"ignoring value - {x}")
//     children

// newElement

let appendHtml element (value: Result<Expression, LigatureError>) =
    match value with
    | Ok(Expression.ObjectView node) ->
        let newElement = createElement node
        emitJsStatement () "element.append(newElement)"
    | x -> failwith $"Unexpected value passed to appendHtml {printResult x}"

let runAndAppendHtml element script =
    let res = runWithDefaults script
    appendHtml element res

let runAndGenerateHtml script =
    match runWithDefaults script with
    | Ok(Expression.ObjectView html) -> Wander.Fns.Html.generateHtml html
    | x -> failwith $"Unexpected value passed to runAndGenerateHtml {x}"

let equivalent left right = Definition.Equivalent left, right

let concept name = ConceptExpr.AtomicConcept name

[<Emit("() => { if (typeof $0 === 'string') { return concept($0)} else { return $0 } }")>]
let handleConcept concept = jsNative

let ``and`` left right =
    ConceptExpr.And [ handleConcept left; handleConcept right ]

let not concept = ConceptExpr.Not(handleConcept concept)

let instance individual concept =
    Assertion.Instance individual, handleConcept concept

let isConsistent tBox aBox : bool =
    match Ligature.Interpreter.isConsistent tBox aBox with
    | Ok value -> value
    | Error err -> failwith err.UserMessage

let termToJs (Term term) =
    let obj = createEmpty
    obj?``type`` <- "Term"
    obj?value <- term
    obj

let elementToJs (element: Element) =
    let obj = createEmpty
    obj?``type`` <- "Element"
    let (Term value) = element.value
    obj?value <- value

    match element.space with
    | Some(Term t) -> obj?``namespace`` <- t
    | _ -> ()

    match element.langTag with
    | Some(Term t) -> obj?langTag <- t
    | _ -> ()

    obj

let rec objectViewToJs (view: ObjectView) =
    let obj = elementToJs view.root

    if view.links.IsEmpty then
        ()
    else
        let links = createEmpty

        Map.iter
            (fun (Term role) fillers ->
                let fillers = List.map (fun value -> objectViewToJs value) fillers |> Array.ofList
                links?(role) <- fillers)
            view.links

        obj?links <- links

    obj

let run script =
    match runWithDefaults script with
    | Ok(Expression.ObjectView view) -> objectViewToJs view
    | Ok(Expression.Term t) -> termToJs t
    | Ok(Expression.Element element) -> elementToJs element
    | x -> failwith $"Unexpected value {x}"
