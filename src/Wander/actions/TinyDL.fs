// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.TinyDL

open Ligature.Model
open Wander.Model
open Wander.Interpreter
open TinyDL.Model
open TinyDL.Core

[<RequireQualifiedAccess>]
type JsonViewValue =
    | Literal of string
    | Term of JsonView

and JsonView =
    { Id: string
      Attrs: Map<string, Set<JsonViewValue>> }

let rec writeValues (values: Set<JsonViewValue>) : string =
    Set.fold
        (fun state value ->
            let value =
                match value with
                | JsonViewValue.Term element -> writeJsonView element
                | JsonViewValue.Literal literal -> encodeString literal

            if state = "[" then state + value else state + "," + value)
        "["
        values
    + "]"

and writeJsonView (view: JsonView) : string =
    let mutable res = "{"

    res <- res + $"\"id\":{encodeString view.Id}"

    Map.iter (fun key values -> res <- res + $",{encodeString key}:{writeValues values}") view.Attrs

    res <- res + "}"
    res

let extract (id: Term) (source: Network) : Record =
    let mutable result = Map.empty
    result <- Map.add (Any.Term(Term "@")) (Any.Term id) result

    Set.iter
        (fun triple ->
            match triple with
            | e, a, Value.Term v ->
                if e = id then
                    result <- Map.add (Any.Term a) (Any.Term v) result
            | e, a, Value.Literal v ->
                if e = id then
                    result <- Map.add (Any.Term a) (Any.Literal v) result)
        source

    result

let extractFn: Fn =
    Fn(
        { doc = "Create a Record of a single invidual's relations."
          examples = [ "extract a (network [a b c])" ]
          args = "Term Network"
          result = "Record" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term id; Any.Network source ] -> Ok(Any.Record(extract id source))
            | _ -> error "Invalid call to extract." None
    )

let instances (source: Network) (concept: Term) : AnySet =
    Set.fold
        (fun state triple ->
            match triple with
            | element, Term ":", conceptToCheck ->
                if conceptToCheck = Value.Term concept then
                    Set.add (Any.Record(extract element source)) state
                else
                    state
            | _ -> state)
        Set.empty
        source

let instancesFn: Fn =
    Fn(
        { doc = "..."
          examples = []
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term concept; Any.Network source ] ->
                let result: AnySet = instances source concept
                Ok(Any.AnySet result)
            | [ Any.Tuple concepts; Any.Network source ] ->
                let result: AnySet =
                    List.fold
                        (fun state concept ->
                            match concept with
                            | Any.Term concept -> instances source concept
                            | _ -> failwith "TODO")
                        Set.empty
                        concepts

                Ok(Any.AnySet result)
            | _ -> failwith "TODO"
    )

let isConsistentFn =
    Fn(
        { doc = "Check if a Network is consistent."
          examples = [ "(is-consistent (define (subconcept A B)) (network [a : A]))" ]
          args = "Definition Network Quote"
          result = "Literal" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Definitions def; Any.Network n ] ->
                match isConsistent def n with
                | Ok true -> Ok(Any.Term(Term "true"))
                | Ok false -> Ok(Any.Term(Term "false"))
                | Error err -> Error err
            | _ -> error "Network on stack required to call count." None
    )

// let inferFn: Fn =
//     Fn(
//         { doc = "..."
//           examples = []
//           args = ""
//           result = "" },
//         fun _ _ _ arguments ->
//             match arguments with
//             | [ description; network ] ->
//                 let description =
//                     match description with
//                     | Any.Definitions n -> n
//                     | _ -> failwith "TODO"

//                 let network =
//                     match network with
//                     | Any.Network n -> n
//                     | _ -> failwith "TODO"

//                 match infer description network with
//                 | Ok res -> Ok(Any.Network res)
//                 | Error err -> error $"Error calling infer: {err}" None
//             | _ -> error "Improper call to infer." None
//     )

let subconceptFn: Fn =
    Fn(
        { doc = "Create a subconcept axiom."
          examples = [ "(subconcept Dog Animal)" ]
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term subconcept; Any.Term concept ] ->
                Ok(Any.Definition(Definition.Subconcept(subconcept, AtomicConcept concept)))
            | _ -> error "Improper call to subconcept." None
    )

let defineFn: Fn =
    Fn(
        { doc = "Define a TBox."
          examples = [ "(define (subconcept Dog Animal))" ]
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            List.fold
                (fun state value ->
                    match state with
                    | Ok(Any.Definitions state) ->
                        match value with
                        | Any.Definition def -> Ok(Any.Definitions(Set.add def state))
                        | _ -> failwith "TODO"
                    | Ok _ -> failwith "Unexpected value."
                    | Error err -> Error err)
                (Ok(Any.Definitions Set.empty))
                arguments
    )
