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
                readBinding (PatternIdentifier.Identifier(Identifier("left"))) currentNetwork

            let right =
                readBinding (PatternIdentifier.Identifier(Identifier("right"))) currentNetwork

            let out =
                readBinding (PatternIdentifier.Identifier(Identifier("out"))) currentNetwork

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
                readBinding (PatternIdentifier.Identifier(Identifier("left"))) currentNetwork

            let right =
                readBinding (PatternIdentifier.Identifier(Identifier("right"))) currentNetwork

            let out =
                readBinding (PatternIdentifier.Identifier(Identifier("out"))) currentNetwork

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

            let data =
                readBinding (PatternIdentifier.Identifier(Identifier("data"))) currentNetwork

            let template =
                readBinding (PatternIdentifier.Identifier(Identifier("template"))) currentNetwork

            let out =
                readBinding (PatternIdentifier.Identifier(Identifier("out"))) currentNetwork

            match (data, template, out) with
            | (Some(LigatureValue.NetworkName(data)),
               Some(LigatureValue.NetworkName(template)),
               Some(LigatureValue.NetworkName(out))) ->
                let dataNetwork = readNetwork data inputState
                let templateNetwork = readNetwork template inputState
                let outNetwork = readNetwork out inputState

                let resultNetwork =
                    Set.map
                        (fun ((e, a, v)) ->
                            let entity =
                                match e with
                                | PatternIdentifier.Slot s ->
                                    match readBinding (PatternIdentifier.Slot s) dataNetwork with
                                    | Some(LigatureValue.Identifier(i)) -> PatternIdentifier.Identifier i
                                    | Some(LigatureValue.Slot(s)) -> PatternIdentifier.Slot s
                                    | Some _ ->
                                        failwith "TODO - unexpected value - only Slots or Identifiers are allowed"
                                    | None -> PatternIdentifier.Slot s
                                | PatternIdentifier.Identifier i -> PatternIdentifier.Identifier i

                            let attribute =
                                match a with
                                | PatternIdentifier.Slot s ->
                                    match readBinding (PatternIdentifier.Slot s) dataNetwork with
                                    | Some(LigatureValue.Identifier(i)) -> PatternIdentifier.Identifier i
                                    | Some(LigatureValue.Slot(s)) -> PatternIdentifier.Slot s
                                    | Some _ ->
                                        failwith "TODO - unexpected value - only Slots or Identifiers are allowed"
                                    | None -> PatternIdentifier.Slot s
                                | PatternIdentifier.Identifier i -> PatternIdentifier.Identifier i

                            let value =
                                match v with
                                | LigatureValue.Slot s ->
                                    match readBinding (PatternIdentifier.Slot s) dataNetwork with
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
                readBinding (PatternIdentifier.Identifier(Identifier("given"))) currentNetwork

            let expect =
                readBinding (PatternIdentifier.Identifier(Identifier("expect"))) currentNetwork

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


let educeCombinator = 
    { Name = "educe"
      Eval =
        fun (inputState: State) ->
            let (networkName, networks) = inputState
            let currentNetwork = currentNetwork inputState

            let src =
                readBinding (PatternIdentifier.Identifier(Identifier("in"))) currentNetwork

            let pattern =
                readBinding (PatternIdentifier.Identifier(Identifier("pattern"))) currentNetwork

            let out =
                readBinding (PatternIdentifier.Identifier(Identifier("out"))) currentNetwork

            match (src, pattern, out) with
            | (Some(LigatureValue.NetworkName(src)),
               Some(LigatureValue.NetworkName(pattern)),
               Some(LigatureValue.NetworkName(out))) ->
                let srcNetwork = readNetwork src inputState
                let patternNetwork = readNetwork pattern inputState
                let outNetwork = readNetwork out inputState

                let resultNetwork = failwith "TODO"
                Ok(networkName, Map.add out (Set.union outNetwork resultNetwork) networks)
            | _ -> failwith "TODO" }


let queryCombinator = 
    { Name = "query"
      Eval =
        fun (inputState: State) ->
            let (networkName, networks) = inputState
            let currentNetwork = currentNetwork inputState

            let src =
                readBinding (PatternIdentifier.Identifier(Identifier("src"))) currentNetwork

            let template =
                readBinding (PatternIdentifier.Identifier(Identifier("template"))) currentNetwork

            let pattern =
                readBinding (PatternIdentifier.Identifier(Identifier("pattern"))) currentNetwork

            let out =
                readBinding (PatternIdentifier.Identifier(Identifier("out"))) currentNetwork

            match (src, template, pattern, out) with
            | (Some(LigatureValue.NetworkName(src)),
               Some(LigatureValue.NetworkName(template)),
               Some(LigatureValue.NetworkName(pattern)),
               Some(LigatureValue.NetworkName(out))) ->
                let srcNetwork = readNetwork src inputState
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
                [ (PatternIdentifier.Identifier(Identifier("id")),
                   PatternIdentifier.Identifier(Identifier("=")),
                   LigatureValue.HostCombinator idCombinator)
                  (PatternIdentifier.Identifier(Identifier("union")),
                   PatternIdentifier.Identifier(Identifier("=")),
                   LigatureValue.HostCombinator unionCombinator)
                  (PatternIdentifier.Identifier(Identifier("assert-equal")),
                   PatternIdentifier.Identifier(Identifier("=")),
                   LigatureValue.HostCombinator assertEqualCombinator)
                  (PatternIdentifier.Identifier(Identifier("educe")),
                   PatternIdentifier.Identifier(Identifier("=")),
                   LigatureValue.HostCombinator queryCombinator)
                  (PatternIdentifier.Identifier(Identifier("query")),
                   PatternIdentifier.Identifier(Identifier("=")),
                   LigatureValue.HostCombinator queryCombinator)
                  (PatternIdentifier.Identifier(Identifier("clear")),
                   PatternIdentifier.Identifier(Identifier("=")),
                   LigatureValue.HostCombinator clearCombinator)
                  (PatternIdentifier.Identifier(Identifier("apply")),
                   PatternIdentifier.Identifier(Identifier("=")),
                   LigatureValue.HostCombinator applyCombinator)
                  (PatternIdentifier.Identifier(Identifier("minus")),
                   PatternIdentifier.Identifier(Identifier("=")),
                   LigatureValue.HostCombinator minusCombinator) ]) ]
     ))
