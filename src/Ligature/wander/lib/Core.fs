// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Core

open Ligature.Main

let idCombinator: Combinator =
    { Name = Symbol("id")
      Doc = "Return the value passed."
      Signature = [ LigatureType.Any ], Some LigatureType.Any
      Eval =
        fun _ _ arguments ->
            match arguments with
            | [ value ] -> Ok(Some(value))
            | _ -> failwith "TODO" }

let setCombinator: Combinator =
    { Name = Symbol("set")
      Doc = "Set the value of a given Network."
      Signature = [ LigatureType.Symbol; LigatureType.Network ], None
      Eval =
        fun _ store arguments ->
            match arguments with
            | [ WanderValue.Symbol(name); WanderValue.Network(value) ] ->
                store.Set name value |> ignore
                Ok(None)
            | _ -> failwith "TODO" }

let readCombinator: Combinator =
    { Name = Symbol("read")
      Doc = "Read the value of a given Network."
      Signature = [ LigatureType.Symbol ], Some LigatureType.Network
      Eval =
        fun _ store arguments ->
            match arguments with
            | [ WanderValue.Symbol(name) ] ->
                let network = WanderValue.Network(store.Read name)
                Ok(Some(network))
            // match Map.tryFind name networks with
            // | Some(network) -> Ok(selected, networks, Some(Identifier.Network network))
            // | _ -> failwith "TODO"
            | _ -> failwith "TODO" }

let ignoreCombinator: Combinator =
    { Name = Symbol("ignore")
      Doc = "Ignore any arguments passed and return working state unchanged."
      Signature = [ LigatureType.Any ], None
      Eval = fun _ networks _ -> Ok(None) }

let printSignature ((arguments, result): LigatureType list * LigatureType option) : Symbol =
    Symbol($"{arguments} -> {result}")
// List.map
//     (fun t ->
//         match t with
//         | LigatureType.Bytes -> Identifier.Name(Name("Bytes"))
//         | LigatureType.Int -> Identifier.Name(Name("Int"))
//         | LigatureType.Name -> Identifier.Name(Name("Name"))
//         | LigatureType.Network -> Identifier.Name(Name("Network"))
//         | LigatureType.NetworkName -> Identifier.Name(Name("NetworkName"))
//         | LigatureType.Quote -> Identifier.Name(Name("Quote"))
//         | LigatureType.Slot -> Identifier.Name(Name("Slot"))
//         | LigatureType.String -> Identifier.Name(Name("String"))
//         | LigatureType.Expression -> Identifier.Name(Name("Expression"))
//         | LigatureType.Value -> Identifier.Name(Name("Value")))
//     signature

let docsCombinator: Combinator =
    { Name = Symbol("docs")
      Doc = "Create a network that contains documentation for the available combinators."
      Signature = [], Some(LigatureType.Network)
      Eval =
        fun combinators networks _ ->
            let mutable docs = Set.empty

            Map.toList combinators
            |> List.iter (fun (name, combinator) ->
                let signature = printSignature combinator.Signature
                docs <- Set.add (name, Symbol("docString"), Symbol(combinator.Doc)) docs
                docs <- Set.add (name, Symbol("signature"), signature) docs)

            Ok(Some(WanderValue.Network docs)) }

let coreCombinators =
    (Map.ofList
        [ (docsCombinator.Name, docsCombinator)
          (idCombinator.Name, idCombinator)
          (ignoreCombinator.Name, ignoreCombinator)
          (readCombinator.Name, readCombinator)
          (setCombinator.Name, setCombinator) ])
