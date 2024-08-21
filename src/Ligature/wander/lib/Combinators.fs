// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Combinators

open Ligature.Main

let idCombinator: Combinator =
    { Name = "id"
      Eval = fun (input: State) (_: Arguments) -> Ok input }

let unionCombinator =
    { Name = "union"
      Eval =
        fun (inputState: State) (arguments: Arguments) ->

            let input =
                match readArgument (Name("in")) arguments with
                | Some(LigatureValue.Network(input)) -> Some input
                | Some(LigatureValue.Name(name)) ->
                    match readBinding (PatternName.Name(name)) inputState with
                    | Some(LigatureValue.Network n) -> Some n
                    | _ -> None
                | _ -> None

            match input with
            | Some(network) -> Set.union network inputState |> Ok
            | _ -> failwith "TODO" }

let minusCombinator =
    { Name = "minus"
      Eval =
        fun (inputState: State) (arguments: Arguments) ->

            let input =
                match readArgument (Name("in")) arguments with
                | Some(LigatureValue.Network(input)) -> Some input
                | Some(LigatureValue.Name(name)) ->
                    match readBinding (PatternName.Name(name)) inputState with
                    | Some(LigatureValue.Network n) -> Some n
                    | _ -> None
                | _ -> None

            match input with
            | Some(network) -> Set.difference inputState network |> Ok
            | _ -> failwith "TODO" }

// let applyCombinator: Combinator =
//     { Name = "apply"
//       Eval =
//         fun (inputState: State) ->
//             let (networkName, networks) = inputState
//             let currentNetwork = currentNetwork inputState

//             let input =
//                 readBinding (PatternName.Name(Name("in"))) currentNetwork

//             let template =
//                 readBinding (PatternName.Name(Name("template"))) currentNetwork

//             let out =
//                 readBinding (PatternName.Name(Name("out"))) currentNetwork

//             match (input, template, out) with
//             | (Some(LigatureValue.QualifiedName(networkName, name)),
//                Some(LigatureValue.NetworkName(template)),
//                Some(LigatureValue.NetworkName(out))) ->
//                 let inputNetwork = readNetwork networkName inputState
//                 let templateNetwork = readNetwork template inputState
//                 let outNetwork = readNetwork out inputState

//                 match readBinding (PatternName.Name(name)) inputNetwork with
//                 | Some(LigatureValue.Quote(values)) ->

//                     failwith "TODO"
//                 | _ -> failwith "TODO"

//                 let resultNetwork =
//                     Set.map
//                         (fun ((e, a, v)) ->
//                             let entity =
//                                 match e with
//                                 | PatternName.Slot s ->
//                                     match readBinding (PatternName.Slot s) dataNetwork with
//                                     | Some(LigatureValue.Name(i)) -> PatternName.Name i
//                                     | Some(LigatureValue.Slot(s)) -> PatternName.Slot s
//                                     | Some _ ->
//                                         failwith "TODO - unexpected value - only Slots or Names are allowed"
//                                     | None -> PatternName.Slot s
//                                 | PatternName.Name i -> PatternName.Name i

//                             let attribute =
//                                 match a with
//                                 | PatternName.Slot s ->
//                                     match readBinding (PatternName.Slot s) dataNetwork with
//                                     | Some(LigatureValue.Name(i)) -> PatternName.Name i
//                                     | Some(LigatureValue.Slot(s)) -> PatternName.Slot s
//                                     | Some _ ->
//                                         failwith "TODO - unexpected value - only Slots or Names are allowed"
//                                     | None -> PatternName.Slot s
//                                 | PatternName.Name i -> PatternName.Name i

//                             let value =
//                                 match v with
//                                 | LigatureValue.Slot s ->
//                                     match readBinding (PatternName.Slot s) dataNetwork with
//                                     | Some value -> value
//                                     | None -> LigatureValue.Slot s
//                                 | v -> v

//                             (entity, attribute, value))
//                         templateNetwork

//                 Ok(networkName, Map.add out (Set.union outNetwork resultNetwork) networks)
//             //iterate through all of the statements in the templateNetwork,
//             //whenever a slot is found lookup in the dataNetwork to see if that value has been bound,
//             //if so replace that slot with the binding and merge the final result into the outNetwork
//             | _ -> failwith "TODO" }

let assertEqualCombinator =
    { Name = "assert-equal"
      Eval =
        fun (inputState: State) (arguments: Arguments) ->

            let given =
                match readArgument (Name("given")) arguments with
                | Some(LigatureValue.Network(given)) -> Some given
                | Some(LigatureValue.Name(name)) ->
                    match readBinding (PatternName.Name(name)) inputState with
                    | Some(LigatureValue.Network n) -> Some n
                    | _ -> None
                | _ -> None

            let expect =
                match readArgument (Name("expect")) arguments with
                | Some(LigatureValue.Network(expect)) -> Some expect
                | Some(LigatureValue.Name(name)) ->
                    match readBinding (PatternName.Name(name)) inputState with
                    | Some(LigatureValue.Network n) -> Some n
                    | _ -> None
                | _ -> None

            match (given, expect) with
            | (Some(given), Some(expect)) ->
                if given = expect then
                    Ok inputState
                else
                    error "Assertion failed." None
            | _ -> failwith "TODO" }

// let queryCombinator =
//     { Name = "query"
//       Eval =
//         fun (inputState: State) ->
//             let (networkName, networks) = inputState
//             let currentNetwork = currentNetwork inputState

//             let input =
//                 readBinding (PatternName.Name(Name("in"))) currentNetwork

//             let template =
//                 readBinding (PatternName.Name(Name("template"))) currentNetwork

//             let pattern =
//                 readBinding (PatternName.Name(Name("pattern"))) currentNetwork

//             let out =
//                 readBinding (PatternName.Name(Name("out"))) currentNetwork

//             match (input, template, pattern, out) with
//             | (Some(LigatureValue.NetworkName(input)),
//                Some(LigatureValue.NetworkName(template)),
//                Some(LigatureValue.NetworkName(pattern)),
//                Some(LigatureValue.NetworkName(out))) ->
//                 let inputNetwork = readNetwork input inputState
//                 let templateNetwork = readNetwork template inputState
//                 let patternNetwork = readNetwork pattern inputState
//                 let outNetwork = readNetwork out inputState

//                 let resultNetwork = failwith "TODO"
//                 Ok(networkName, Map.add out (Set.union outNetwork resultNetwork) networks)
//             | _ -> failwith "TODO" }

let stdState: State =
    Set.ofSeq
        [ (PatternName.Name(Name("id")), PatternName.Name(Name("=")), LigatureValue.HostCombinator idCombinator)
          (PatternName.Name(Name("union")),
           PatternName.Name(Name("=")),
           LigatureValue.HostCombinator unionCombinator)
          (PatternName.Name(Name("assert-equal")),
           PatternName.Name(Name("=")),
           LigatureValue.HostCombinator assertEqualCombinator)
          (PatternName.Name(Name("minus")),
           PatternName.Name(Name("=")),
           LigatureValue.HostCombinator minusCombinator) ]
// (PatternName.Name(Name("query")),
//  PatternName.Name(Name("=")),
//  LigatureValue.HostCombinator queryCombinator)
// (PatternName.Name(Name("apply")),
//  PatternName.Name(Name("=")),
//  LigatureValue.HostCombinator applyCombinator)
