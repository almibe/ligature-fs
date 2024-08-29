// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Combinators

open Ligature.Main
open Ligature.InMemoryNetwork
open Interpreter

let ignoreCombinator: Combinator =
    { Name = Name("ignore")
      Doc = "Ignore any arguments passed and return working state unchanged."
      Eval = fun _ (input: State) _ -> Ok(input, None) }

let assertEqualCombinator: Combinator =
    { Name = Name "assert-equal"
      Doc = ""
      Eval =
        fun (combinators: Combinators) (inputState: State) (arguments: Arguments) ->
            match arguments with
            | [ first; second ] ->
                let first =
                    match first with
                    | LigatureValue.Expression e ->
                        match evalExpression combinators inputState e with
                        | Ok(_, Some(res)) -> res
                        | Error err -> failwith $"Expression errored: {err.UserMessage}."
                    | _ -> first

                let second =
                    match second with
                    | LigatureValue.Expression e ->
                        match evalExpression combinators inputState e with
                        | Ok(_, Some(res)) -> res
                        | _ -> failwith "TODO"
                    | _ -> second

                if first = second then
                    Ok(inputState, Some(LigatureValue.String("Sucess!")))
                else
                    error
                        $"assert-equal failed {Ligature.Wander.Model.prettyPrint first} != {Ligature.Wander.Model.prettyPrint second}"
                        None
            | args -> error $"assert-equal passed illegal arguments - {args}" None }

// let unionCombinator =
//     { Name = "union"
//       Eval =
//         fun (inputState: State) (arguments: Arguments) ->

//             let input =
//                 match readArgument (Name("in")) arguments with
//                 | Some(LigatureValue.Network(input)) -> Some input
//                 | Some(LigatureValue.Name(name)) ->
//                     match readBinding (PatternName.Name(name)) inputState with
//                     | Some(LigatureValue.Network n) -> Some n
//                     | _ -> None
//                 | _ -> None

//             match input with
//             | Some(network) -> Set.union network inputState |> Ok
//             | _ -> failwith "TODO" }

// let minusCombinator =
//     { Name = "minus"
//       Eval =
//         fun (inputState: State) (arguments: Arguments) ->

//             let input =
//                 match readArgument (Name("in")) arguments with
//                 | Some(LigatureValue.Network(input)) -> Some input
//                 | Some(LigatureValue.Name(name)) ->
//                     match readBinding (PatternName.Name(name)) inputState with
//                     | Some(LigatureValue.Network n) -> Some n
//                     | _ -> None
//                 | _ -> None

//             match input with
//             | Some(network) -> Set.difference inputState network |> Ok
//             | _ -> failwith "TODO" }

// let lookupNetwork () =
//     failwith "TODO"

let applyNetwork (template: Network) (data: Network): Network =
    Set.map
        (fun ((e, a, v)) ->
            let entity =
                match e with
                | PatternName.Slot s ->
                    match readBinding (PatternName.Slot s) data with
                    | Some(LigatureValue.Name(i)) -> PatternName.Name i
                    | Some(LigatureValue.Slot(s)) -> PatternName.Slot s
                    | Some _ ->
                        failwith "TODO - unexpected value - only Slots or Names are allowed"
                    | None -> PatternName.Slot s
                | PatternName.Name i -> PatternName.Name i

            let attribute =
                match a with
                | PatternName.Slot s ->
                    match readBinding (PatternName.Slot s) data with
                    | Some(LigatureValue.Name(i)) -> PatternName.Name i
                    | Some(LigatureValue.Slot(s)) -> PatternName.Slot s
                    | Some _ ->
                        failwith "TODO - unexpected value - only Slots or Names are allowed"
                    | None -> PatternName.Slot s
                | PatternName.Name i -> PatternName.Name i

            let value =
                match v with
                | LigatureValue.Slot s ->
                    match readBinding (PatternName.Slot s) data with
                    | Some value -> value
                    | None -> LigatureValue.Slot s
                | v -> v

            (entity, attribute, value))
        template


let applyCombinator: Combinator =
    { Name = Name("apply")
      Doc = ""
      Eval =
        fun combinators (inputState: State) arguments ->
            match arguments with
            | [LigatureValue.Network(template)
               LigatureValue.Quote(data)] ->
                List.map (fun dataset ->
                    match dataset with
                    | LigatureValue.Network dataset -> 
                        applyNetwork template dataset
                        |> LigatureValue.Network
                    | _ -> failwith "TODO") data
                |> LigatureValue.Quote
                |> fun value -> Ok(inputState, Some(value))
            | _ -> failwith "TODO" }

let matchCombinator =
    { Name = Name("match")
      Doc = "args: pattern data\nreturns: quote of networks"
      Eval =
        fun _ (inputState: State) arguments ->
            let (networkName, networks) = inputState
            let currentNetwork = currentNetwork inputState

            match arguments with
            | [ LigatureValue.Network(pattern); LigatureValue.Network(data) ] ->
                let res = matchNetwork pattern data
                Ok(inputState, Some(res))
            | _ -> error "" None }

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

let stdCombinators =
    Map.ofList
        [ (applyCombinator.Name, (applyCombinator))
          (assertEqualCombinator.Name, (assertEqualCombinator))
          (ignoreCombinator.Name, (ignoreCombinator))
          (matchCombinator.Name, (matchCombinator)) ]

// [ (PatternName.Name(Name("id")), PatternName.Name(Name("=")), LigatureValue.HostCombinator idCombinator)
//   (PatternName.Name(Name("union")),
//    PatternName.Name(Name("=")),
//    LigatureValue.HostCombinator unionCombinator)
//   (PatternName.Name(Name("assert-equal")),
//    PatternName.Name(Name("=")),
//    LigatureValue.HostCombinator assertEqualCombinator)
//   (PatternName.Name(Name("minus")),
//    PatternName.Name(Name("=")),
//    LigatureValue.HostCombinator minusCombinator)
//   (PatternName.Name(Name("apply")),
//    PatternName.Name(Name("=")),
//    LigatureValue.HostCombinator applyCombinator)
// (PatternName.Name(Name("query")),
//  PatternName.Name(Name("=")),
//  LigatureValue.HostCombinator queryCombinator)
//        ]
