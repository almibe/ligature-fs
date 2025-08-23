open Wander.Main
open Ligature.InMemoryStore
open Ligature.Model
open Ligature.Interpreter
open Wander.Model
open System.Collections.Generic
open Wander.Library
open Fable.Core
open Fable.Core.JsInterop

let printResult = printResult

let ok value = Ok value

let error value = Error value

let termToJs (Term term) =
    let obj = createEmpty
    obj?``type`` <- "Element"
    obj?value <- term
    obj

let unitToJs () =
    let obj = createEmpty
    obj?``type`` <- "Unit"
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

type ElementView =
    { root: Element
      links: Map<string, Element list> }

let elementViewsToJs (views: ElementView seq) : obj array =
    Seq.map
        (fun view ->
            let result = elementToJs view.root
            result?``type`` <- "ElementView"
            let mutable links: Map<string, obj> = Map.empty

            Map.iter
                (fun key value ->
                    let values = Seq.map (fun value -> elementToJs value) value |> Array.ofSeq

                    links <- links.Add(key, values))
                view.links

            let finalLinks = createEmpty

            Map.iter (fun key value -> finalLinks?(key) <- value) links

            result?links <- finalLinks
            result)
        views
    |> Seq.toArray

let assertionsToElementViews (assertions: Assertions) : ElementView seq =
    let mutable processing: Map<Element, ElementView> = Map.empty

    Set.iter
        (fun value ->
            match value with
            | Assertion.Triple(individual, Term role, filler) ->
                match processing.TryFind individual with
                | Some view ->
                    let newFillers =
                        match view.links.TryFind role with
                        | Some fillers -> List.append fillers [ filler ]
                        | None -> [ filler ]

                    let newView =
                        { view with
                            links = Map.add role newFillers view.links }

                    processing <- Map.add individual newView processing
                | None ->
                    let newView =
                        { root = individual
                          links = Map.ofList [ role, [ filler ] ] }

                    processing <- Map.add individual newView processing
            | Assertion.Instance(individual, ConceptExpr.AtomicConcept(Term c)) -> ()
            | _ -> ())
        assertions

    processing.Values

let resultToJs result =
    match result with
    | Ok(Expression.Term t) -> termToJs t
    | Ok(Expression.Element element) -> elementToJs element
    | Ok(Expression.Assertions assertions) -> assertionsToElementViews assertions |> elementViewsToJs
    | Ok Expression.Unit -> unitToJs ()
    | x -> failwith $"Unexpected value {x}"

let appendText (text: string) htmlElement =
    let codeEl = emitJsExpr () "document.createElement('code')"
    let preEl = emitJsExpr () "document.createElement('pre')"

    emitJsStatement
        (htmlElement, codeEl, preEl, text)
        "
        $2.appendChild($1)
        $0.appendChild($2)
        $1.textContent = $3
    "

let runWithFns (fns: Dictionary<string, obj -> unit>) (script: string) htmlElement =
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
                    fun _ _ application ->
                        match application.arguments with
                        | [ Expression.Assertions assertions ] ->
                            let elementViews = assertionsToElementViews assertions |> elementViewsToJs
                            entry.Value elementViews |> ignore
                            Ok Expression.Unit
                        | [ Expression.Element element ] ->
                            entry.Value(elementToJs element) |> ignore
                            Ok Expression.Unit
                        | x -> failwith $"Unexpected value passed to {entry.Key} - {x}"
                ))
                resFns

    resFns <-
        Map.add
            (Term "print")
            (Fn.Fn(
                { doc = "Print a value."
                  examples = []
                  args = ""
                  result = "" },
                fun _ _ application ->
                    match application.arguments with
                    | [ arg ] ->
                        appendText $"{printExpression arg}" htmlElement
                        Ok Expression.Unit
                    | _ -> failwith "Unexpected arguments to print."
            ))
            resFns

    run resFns Map.empty script

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
