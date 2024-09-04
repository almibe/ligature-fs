// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Core

open Ligature.Main
open Ligature.InMemoryNetwork

let idCombinator: Combinator =
    { Name = Name("id")
      Doc = "Return the value passed."
      Signature = [ LigatureType.Any ], Some LigatureType.Any
      Eval =
        fun _ name networks arguments ->
            match arguments with
            | [ value ] -> Ok(name, networks, Some(value))
            | _ -> failwith "TODO" }

let setCombinator: Combinator =
    { Name = Name("set")
      Doc = "Set the value of a given Network."
      Signature = [ LigatureType.NetworkName; LigatureType.Network ], None
      Eval =
        fun _ selected networks arguments ->
            match arguments with
            | [ LigatureValue.NetworkName(name); LigatureValue.Network(value) ] ->
                let newNetworks = Map.add name value networks
                Ok(selected, newNetworks, None)
            | _ -> failwith "TODO" }

let readCombinator: Combinator =
    { Name = Name("read")
      Doc = "Read the value of a given Network."
      Signature = [ LigatureType.NetworkName ], Some LigatureType.Network
      Eval =
        fun _ selected networks arguments ->
            match arguments with
            | [ LigatureValue.NetworkName(name) ] ->
                match Map.tryFind name networks with
                | Some(network) -> Ok(selected, networks, Some(LigatureValue.Network network))
                | _ -> failwith "TODO"
            | _ -> failwith "TODO" }

let ignoreCombinator: Combinator =
    { Name = Name("ignore")
      Doc = "Ignore any arguments passed and return working state unchanged."
      Signature = [ LigatureType.Any ], None
      Eval = fun _ name networks _ -> Ok(name, networks, None) }

let printSignature ((arguments, result): LigatureType list * LigatureType option) : LigatureValue =
    LigatureValue.String($"{arguments} -> {result}")
// List.map
//     (fun t ->
//         match t with
//         | LigatureType.Bytes -> LigatureValue.Name(Name("Bytes"))
//         | LigatureType.Int -> LigatureValue.Name(Name("Int"))
//         | LigatureType.Name -> LigatureValue.Name(Name("Name"))
//         | LigatureType.Network -> LigatureValue.Name(Name("Network"))
//         | LigatureType.NetworkName -> LigatureValue.Name(Name("NetworkName"))
//         | LigatureType.Quote -> LigatureValue.Name(Name("Quote"))
//         | LigatureType.Slot -> LigatureValue.Name(Name("Slot"))
//         | LigatureType.String -> LigatureValue.Name(Name("String"))
//         | LigatureType.Expression -> LigatureValue.Name(Name("Expression"))
//         | LigatureType.Value -> LigatureValue.Name(Name("Value")))
//     signature

let docsCombinator: Combinator =
    { Name = Name("docs")
      Doc = "Create a network that contains documentation for the available combinators."
      Signature = [], Some(LigatureType.Network)
      Eval =
        fun combinators name networks _ ->
            let mutable docs = emptyNetwork

            Map.toList combinators
            |> List.iter (fun (name, combinator) ->
                let signature = printSignature combinator.Signature

                docs <-
                    Set.add
                        (PatternName.Name(name),
                         PatternName.Name(Name("docString")),
                         LigatureValue.String(combinator.Doc))
                        docs

                docs <- Set.add (PatternName.Name(name), PatternName.Name(Name("signature")), signature) docs)

            Ok(name, networks, Some(LigatureValue.Network docs)) }

let coreCombinators =
    (Map.ofList
        [ (docsCombinator.Name, docsCombinator)
          (idCombinator.Name, idCombinator)
          (ignoreCombinator.Name, ignoreCombinator)
          (readCombinator.Name, readCombinator)
          (setCombinator.Name, setCombinator) ])
