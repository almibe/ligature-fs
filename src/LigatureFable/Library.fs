﻿open Wander.Main
open Ligature.InMemoryStore
open Ligature.Model
open Ligature.Interpreter
open Wander.Model
open System.Collections.Generic
open Wander.Library
open Fable.Core
open Fable.Core.JsInterop

let runAndPrint = runWithDefaults >> printResult

let printResult = printResult

let printAny = printExpression

let ok value = Ok value

let error value = Error value

let termToJs (Term term) =
    let obj = createEmpty
    obj?``type`` <- "Element"
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

let assertionsToJs (assertions: Assertions) =
    let assertions =
        Set.map
            (fun value ->
                match value with
                | Assertion.Triple(individual, Term role, filler) ->
                    let triple = createEmpty
                    triple?``type`` <- "Triple"

                    triple?element <- elementToJs individual
                    triple?role <- role
                    triple?filler <- elementToJs filler

                    triple
                | Assertion.Instance(individual, ConceptExpr.AtomicConcept(Term c)) ->
                    let instance = createEmpty
                    instance?``type`` <- "Instance"

                    instance?element <- elementToJs individual
                    instance?concept <- c

                    instance)
            assertions

    let assertions = Array.ofSeq assertions
    let obj = createEmpty
    obj?``type`` <- "Assertions"
    obj?assertions <- emitJsExpr assertions "new Set($0)"
    obj

let resultToJs result =
    match result with
    | Ok(Expression.Term t) -> termToJs t
    | Ok(Expression.Element element) -> elementToJs element
    | Ok(Expression.Assertions assertions) -> assertionsToJs assertions
    | x -> failwith $"Unexpected value {x}"

let runWithFns (fns: Dictionary<string, obj -> obj>) (script: string) =
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
                            let assertions = assertionsToJs assertions
                            entry.Value assertions |> ignore
                            Ok Expression.Unit
                        | x -> failwith $"Unexpected value passed to {entry.Key} - {x}"
                ))
                resFns

    run resFns Map.empty script |> resultToJs

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

let run script = runWithDefaults script |> resultToJs

let select (assertions: Assertions) (element: Element) : Assertions =
    let result = elementToJs element
    let links: Dictionary<string, System.Collections.Generic.List<obj>> = Dictionary()

    Set.iter
        (fun assertion ->
            match assertion with
            | Assertion.Triple(condValue, (Term role), filler) when element = condValue ->
                if links.ContainsKey(role) then
                    let _, values = links.TryGetValue role
                    values.Add(elementToJs filler)
                else
                    let values = System.Collections.Generic.List()
                    values.Add(elementToJs filler)
                    links.Add(role, values)
            | Assertion.Instance(condValue, concept) when element = condValue -> failwith "TODO"
            | _ -> failwith "TODO")
        assertions

    result?links <- links
    result

let runAndSelectElement script element =
    match runWithDefaults script with
    | Ok(Expression.Assertions assertions) ->
        printfn "TEST"
        select assertions (el element)
    | _ -> failwith "TODO"

let runAndSelectConcept script concept = failwith "TODO"
// let concept = ConceptExpr.AtomicConcept (Term concept)
// match runWithDefaults script with
// | Ok(Expression.Assertions assertions) -> select assertions concept
// | _ -> failwith "TODO"
