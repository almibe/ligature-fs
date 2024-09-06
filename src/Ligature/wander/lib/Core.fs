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
        fun _ _ arguments ->
            match arguments with
            | [ value ] -> Ok(Some(value))
            | _ -> failwith "TODO" }

let setCombinator: Combinator =
    { Name = Name("set")
      Doc = "Set the value of a given Network."
      Signature = [ LigatureType.Name; LigatureType.Network ], None
      Eval =
        fun _ store arguments ->
            match arguments with
            | [ LigatureValue.Name(name); LigatureValue.Network(value) ] ->
                store.Set name value |> ignore
                Ok(None)
            | _ -> failwith "TODO" }

let readCombinator: Combinator =
    { Name = Name("read")
      Doc = "Read the value of a given Network."
      Signature = [ LigatureType.Name ], Some LigatureType.Network
      Eval =
        fun _ store arguments ->
            match arguments with
            | [ LigatureValue.Name(name) ] ->
                let network = LigatureValue.Network(store.Read name)
                Ok(Some(network))
            // match Map.tryFind name networks with
            // | Some(network) -> Ok(selected, networks, Some(LigatureValue.Network network))
            // | _ -> failwith "TODO"
            | _ -> failwith "TODO" }

let ignoreCombinator: Combinator =
    { Name = Name("ignore")
      Doc = "Ignore any arguments passed and return working state unchanged."
      Signature = [ LigatureType.Any ], None
      Eval = fun _ networks _ -> Ok(None) }

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
        fun combinators networks _ ->
            let mutable docs = Set.empty

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

            Ok(Some(LigatureValue.Network docs)) }

let coreCombinators =
    (Map.ofList
        [ (docsCombinator.Name, docsCombinator)
          (idCombinator.Name, idCombinator)
          (ignoreCombinator.Name, ignoreCombinator)
          (readCombinator.Name, readCombinator)
          (setCombinator.Name, setCombinator) ])
