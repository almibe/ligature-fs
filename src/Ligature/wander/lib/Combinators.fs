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
                //iterate through all of the statements in
                failwith "TODO"
            | _ -> failwith "TODO" }

let stdState: State =
    ("",
     Map.ofSeq (
         [ ("combinators",
            Set.ofSeq
                [ (PatternIdentifier.Identifier(Identifier("id")),
                   PatternIdentifier.Identifier(Identifier("=")),
                   LigatureValue.HostCombinator idCombinator)
                  (PatternIdentifier.Identifier(Identifier("union")),
                   PatternIdentifier.Identifier(Identifier("=")),
                   LigatureValue.HostCombinator unionCombinator)
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
