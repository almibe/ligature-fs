// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.TinyDL

open Ligature.Model
open Wander.Model
open Wander.Interpreter

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

let rec infer (tBox: Network) (aBox: Network) : Result<Network, LigatureError> =
    let mutable res = aBox

    Set.iter
        (fun tStatement ->
            Set.iter
                (fun aStatement ->
                    match tStatement, aStatement with
                    | (subconcept, Term "subconcept-of", superconcept), (element, Term ":", Value.Term concept) when
                        subconcept = concept
                        ->
                        res <- Set.add (element, Term ":", superconcept) res
                    | (firstRole, Term "tiny-dl.inverse-of", Value.Term secondRole), (first, role, Value.Term second) when role = firstRole ->
                        res <- Set.add (second, secondRole, Value.Term first) res
                    | (firstRole, Term "tiny-dl.inverse-of", Value.Term secondRole), (first, role, Value.Term second) when role = secondRole ->
                        res <- Set.add (second, firstRole, Value.Term first) res
                    | (roleName, Term ":", Value.Term (Term "tiny-dl.Is-Symmetrical")), (first, role, Value.Term second) when role = roleName ->
                        res <- Set.add (second, role, Value.Term first) res
                    | _ -> ())
                aBox)
        tBox

    if aBox = res then Ok res else infer tBox res

// let extract (source: Network) (id: Term) : Network =
//     let mutable checkedIds = Set.ofList [ id ]
//     let mutable toCheck = [ id ]
//     let mutable result = Set.empty

//     while not toCheck.IsEmpty do
//         let current = toCheck.Head
//         toCheck <- toCheck.Tail

//         let matched =
//             Set.filter
//                 (fun (element, a, v) ->
//                     if element = current then
//                         match v with
//                         | TermPattern.Term e ->
//                             if not (checkedIds.Contains(e)) then
//                                 toCheck <- e :: toCheck
//                                 checkedIds <- Set.add e checkedIds
//                             else
//                                 ()
//                         | _ -> ()

//                         true
//                     else
//                         false)
//                 source

//         result <- Set.union matched result

//     result

// let extractFn: Fn =
//     Fn(
//         { doc = "..."
//           examples = []
//           pre = ""
//           post = "" },
//         fun actions variables arguments -> failwith "TODO"
//     // match stack with
//     // | Any.Quote ids :: Any.Network source :: tail ->
//     //     let result: AnySet =
//     //         List.fold
//     //             (fun state concept ->
//     //                 match concept with
//     //                 | Any.Term concept -> Set.add (Any.Network(extract source concept)) state
//     //                 | _ -> failwith "TODO")
//     //             (Set.empty)
//     //             ids

//     //     Ok(Any.AnySet result :: tail)
//     // | _ -> failwith "TODO"
//     )

let rec createJsonView (source: Pattern) (Term root) : JsonView =
    failwith "TODO"
    // let mutable attrs = Map.empty

    // Set.iter
    //     (fun triple ->
    //         match triple with
    //         | TermPattern.Term element, TermPattern.Term(Term attribute), value ->
    //             if element = Term root then
    //                 let value =
    //                     match value with
    //                     | TermPattern.Term e -> JsonViewValue.Term(createJsonView source e)
    //                     | _ -> failwith "TODO"

    //                 if attrs.ContainsKey attribute then
    //                     failwith "TODO"
    //                 else
    //                     let values = Set.ofList [ value ]
    //                     attrs <- Map.add attribute values attrs
    //         | _ -> failwith "TODO")
    //     source

    // let view = { Id = root; Attrs = attrs }
    // view

let extractJson (ids: Quote) (source: Pattern) : string = failwith "TODO"
// let mutable result = "["

// List.iteri
//     (fun index id ->
//         match id with
//         | Any.Term id ->
//             if (index > 0) then
//                 result <- result + ","

//             result <- result + (createJsonView (extract source id) id |> writeJsonView)
//         | _ -> failwith "TODO")
//     ids

// result <- result + "]"
// result

let extractJsonFn: Fn =
    Fn(
        { doc = "..."
          examples = []
          args = ""
          result = "" },
        fun actions variables arguments -> failwith "TODO"
    // match stack with
    // | Any.Quote ids :: Any.Network source :: tail ->
    //     let json = extractJson ids source
    //     Ok(Any.Term(Term json) :: tail)
    // | _ -> failwith "TODO"
    )

// let instances (source: Network) (concept: Term) : AnySet =
//     Set.fold
//         (fun state triple ->
//             match triple with
//             | element, Term ":", conceptToCheck ->
//                 if conceptToCheck = concept then
//                     Set.add (Any.Network(extract source element)) state
//                 else
//                     state
//             | _ -> state)
//         Set.empty
//         source

// let instancesFn: Fn =
//     Fn(
//         { doc = "..."
//           examples = []
//           pre = ""
//           post = "" },
//         fun actions variables arguments ->
//             match arguments with
//             | [ Any.Quote concepts; Any.Network source ] ->
//                 let result: AnySet =
//                     List.fold
//                         (fun state concept ->
//                             match concept with
//                             | Any.Term concept -> instances source concept
//                             | _ -> failwith "TODO")
//                         (Set.empty)
//                         concepts

//                 Ok(variables, Any.AnySet result)
//             | _ -> failwith "TODO"
//     )

// let instancesJsonFn: Fn =
//     Fn(
//         { doc = "..."
//           examples = []
//           pre = ""
//           post = "" },
//         fun actions variables arguments -> failwith "TODO"
//     // match stack with
//     // | Any.Quote concepts :: Any.Network source :: tail ->
//     //     let ids: Quote =
//     //         List.fold
//     //             (fun state concept ->
//     //                 let mutable state = state

//     //                 match concept with
//     //                 | Any.Term el ->
//     //                     Set.iter
//     //                         (fun triple ->
//     //                             match triple with
//     //                             | TermPattern.Term e,
//     //                               TermPattern.Term(Term ":"),
//     //                               TermPattern.Term concept ->
//     //                                 if el = concept then
//     //                                     state <- Any.Term e :: state
//     //                             | _ -> ())
//     //                         source

//     //                     state
//     //                 | _ -> failwith "TODO")
//     //             List.empty
//     //             concepts

//     //     let json = extractJson ids source
//     //     Ok(Any.Term(Term json) :: tail)
//     // | _ -> failwith "TODO"
//     )

let isConsistentFn =
    Fn(
        { doc = "Check if a Network is consistent."
          examples = [ "is-consistent {a : B, b : C} | assert-equal true" ]
          args = "Network"
          result = "Literal" },
        fun _ _ arguments ->
            match arguments with
            | [ Any.Network n ] ->
                if
                    Ligature.Core.query
                        (Set.ofList
                            [ TermPattern.Slot(Slot "?el"),
                              TermPattern.Term(Term ":"),
                              ValuePattern.Slot(Slot "?concept")
                              TermPattern.Slot(Slot "?el"),
                              TermPattern.Term(Term "~:"),
                              ValuePattern.Slot(Slot "?concept") ])
                        (Set.ofList
                            [ TermPattern.Slot(Slot "?el"),
                              TermPattern.Term(Term "~:"),
                              ValuePattern.Slot(Slot "?concept") ])
                        n
                    |> Seq.length = 0
                then
                    Ok(Any.Term(Term "true"))
                else
                    Ok(Any.Term(Term "false"))
            | _ -> error "Network on stack required to call count." None
    )

let inferFn: Fn =
    Fn(
        { doc = "..."
          examples = []
          args = ""
          result = "" },
        fun actions variables arguments ->
            match arguments with
            | [ description; network ] ->
                let description =
                    match description with
                    | Any.Network n -> n
                    | _ -> failwith "TODO"

                let network =
                    match network with
                    | Any.Network n -> n
                    | _ -> failwith "TODO"

                match infer description network with
                | Ok res -> Ok(Any.Network res)
                | Error err -> error $"Error calling infer: {err}" None
            | _ -> error "Improper call to infer." None
    )
