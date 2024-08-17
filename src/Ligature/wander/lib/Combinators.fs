// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Combinators

open Ligature.Main

let idCombinator: Combinator =
    { Name = "id"
      Eval = fun (input: State) -> Ok input }

let clearCombinator =
    { Name = "clear"
      Eval = fun ((networkName, networks): State) -> Ok(networkName, (Map.remove networkName networks)) }

let unionCombinator =
    { Name = "union"
      Eval =
        fun (inputState: State) ->
            let networkName, networks = inputState
            let currentNetwork = currentNetwork inputState

            let left =
                readBinding (PatternName.Name(Name("left"))) currentNetwork

            let right =
                readBinding (PatternName.Name(Name("right"))) currentNetwork

            let out =
                readBinding (PatternName.Name(Name("out"))) currentNetwork

            match (left, right, out) with
            | (Some(LigatureValue.NetworkName(left)),
               Some(LigatureValue.NetworkName(right)),
               Some(LigatureValue.NetworkName(out))) ->
                let leftNetwork = readNetwork left inputState
                let rightNetwork = readNetwork right inputState
                let outNetwork = readNetwork out inputState
                let res = Set.union leftNetwork rightNetwork |> Set.union outNetwork
                Ok(networkName, Map.add out res networks)
            | _ -> failwith "TODO" }

let minusCombinator =
    { Name = "minus"
      Eval =
        fun (inputState: State) ->
            let networkName, networks = inputState
            let currentNetwork = currentNetwork inputState

            let left =
                readBinding (PatternName.Name(Name("left"))) currentNetwork

            let right =
                readBinding (PatternName.Name(Name("right"))) currentNetwork

            let out =
                readBinding (PatternName.Name(Name("out"))) currentNetwork

            match (left, right, out) with
            | (Some(LigatureValue.NetworkName(left)),
               Some(LigatureValue.NetworkName(right)),
               Some(LigatureValue.NetworkName(out))) ->
                let leftNetwork = readNetwork left inputState
                let rightNetwork = readNetwork right inputState
                let outNetwork = readNetwork out inputState
                let res = Set.difference leftNetwork rightNetwork |> Set.union outNetwork
                Ok(networkName, Map.add out res networks)
            | _ -> failwith "TODO" }

let applyCombinator: Combinator =
    { Name = "apply"
      Eval =
        fun (inputState: State) ->
            let (networkName, networks) = inputState
            let currentNetwork = currentNetwork inputState
 
            let input =
                readBinding (PatternName.Name(Name("in"))) currentNetwork

            let template =
                readBinding (PatternName.Name(Name("template"))) currentNetwork

            let out =
                readBinding (PatternName.Name(Name("out"))) currentNetwork

            match (input, template, out) with
            | (Some(LigatureValue.QualifiedName(networkName, name)),
               Some(LigatureValue.NetworkName(template)),
               Some(LigatureValue.NetworkName(out))) ->
                let inputNetwork = readNetwork networkName inputState
                let templateNetwork = readNetwork template inputState
                let outNetwork = readNetwork out inputState

                match readBinding (PatternName.Name(name)) inputNetwork with
                | Some(LigatureValue.Quote(values)) -> 
                    
                    failwith "TODO"
                | _ -> failwith "TODO"

                let resultNetwork =
                    Set.map
                        (fun ((e, a, v)) ->
                            let entity =
                                match e with
                                | PatternName.Slot s ->
                                    match readBinding (PatternName.Slot s) dataNetwork with
                                    | Some(LigatureValue.Name(i)) -> PatternName.Name i
                                    | Some(LigatureValue.Slot(s)) -> PatternName.Slot s
                                    | Some _ ->
                                        failwith "TODO - unexpected value - only Slots or Names are allowed"
                                    | None -> PatternName.Slot s
                                | PatternName.Name i -> PatternName.Name i

                            let attribute =
                                match a with
                                | PatternName.Slot s ->
                                    match readBinding (PatternName.Slot s) dataNetwork with
                                    | Some(LigatureValue.Name(i)) -> PatternName.Name i
                                    | Some(LigatureValue.Slot(s)) -> PatternName.Slot s
                                    | Some _ ->
                                        failwith "TODO - unexpected value - only Slots or Names are allowed"
                                    | None -> PatternName.Slot s
                                | PatternName.Name i -> PatternName.Name i

                            let value =
                                match v with
                                | LigatureValue.Slot s ->
                                    match readBinding (PatternName.Slot s) dataNetwork with
                                    | Some value -> value
                                    | None -> LigatureValue.Slot s
                                | v -> v

                            (entity, attribute, value))
                        templateNetwork

                Ok(networkName, Map.add out (Set.union outNetwork resultNetwork) networks)
            //iterate through all of the statements in the templateNetwork,
            //whenever a slot is found lookup in the dataNetwork to see if that value has been bound,
            //if so replace that slot with the binding and merge the final result into the outNetwork
            | _ -> failwith "TODO" }

let assertEqualCombinator = 
    { Name = "assert-equal"
      Eval =
        fun (inputState: State) ->
            let (networkName, networks) = inputState
            let currentNetwork = currentNetwork inputState

            let given =
                readBinding (PatternName.Name(Name("given"))) currentNetwork

            let expect =
                readBinding (PatternName.Name(Name("expect"))) currentNetwork

            match (given, expect) with
            | (Some(LigatureValue.NetworkName(given)),
               Some(LigatureValue.NetworkName(expect))) ->
                let givenNetwork = readNetwork given inputState
                let expectNetwork = readNetwork expect inputState

                if givenNetwork = expectNetwork then
                    Ok inputState
                else
                    error "Assertion failed." None
            | _ -> failwith "TODO" }

let queryCombinator = 
    { Name = "query"
      Eval =
        fun (inputState: State) ->
            let (networkName, networks) = inputState
            let currentNetwork = currentNetwork inputState

            let input =
                readBinding (PatternName.Name(Name("in"))) currentNetwork

            let template =
                readBinding (PatternName.Name(Name("template"))) currentNetwork

            let pattern =
                readBinding (PatternName.Name(Name("pattern"))) currentNetwork

            let out =
                readBinding (PatternName.Name(Name("out"))) currentNetwork

            match (input, template, pattern, out) with
            | (Some(LigatureValue.NetworkName(input)),
               Some(LigatureValue.NetworkName(template)),
               Some(LigatureValue.NetworkName(pattern)),
               Some(LigatureValue.NetworkName(out))) ->
                let inputNetwork = readNetwork input inputState
                let templateNetwork = readNetwork template inputState
                let patternNetwork = readNetwork pattern inputState
                let outNetwork = readNetwork out inputState

                let resultNetwork = failwith "TODO"
                Ok(networkName, Map.add out (Set.union outNetwork resultNetwork) networks)
            | _ -> failwith "TODO" }

let stdState: State =
    (NetworkName(""),
     Map.ofSeq (
         [ (NetworkName("combinators"),
            Set.ofSeq
                [ (PatternName.Name(Name("id")),
                   PatternName.Name(Name("=")),
                   LigatureValue.HostCombinator idCombinator)
                  (PatternName.Name(Name("union")),
                   PatternName.Name(Name("=")),
                   LigatureValue.HostCombinator unionCombinator)
                  (PatternName.Name(Name("assert-equal")),
                   PatternName.Name(Name("=")),
                   LigatureValue.HostCombinator assertEqualCombinator)
                  (PatternName.Name(Name("query")),
                   PatternName.Name(Name("=")),
                   LigatureValue.HostCombinator queryCombinator)
                  (PatternName.Name(Name("clear")),
                   PatternName.Name(Name("=")),
                   LigatureValue.HostCombinator clearCombinator)
                  (PatternName.Name(Name("apply")),
                   PatternName.Name(Name("=")),
                   LigatureValue.HostCombinator applyCombinator)
                  (PatternName.Name(Name("minus")),
                   PatternName.Name(Name("=")),
                   LigatureValue.HostCombinator minusCombinator) ]) ]
     ))
