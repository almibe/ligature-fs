// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Core

open Ligature.Main
open Ligature.InMemoryNetwork

let idCombinator: Combinator =
    { Name = Name("id")
      Doc = "Return the value passed."
      Eval =
        fun _ (input: State) arguments ->
            match arguments with
            | [ value ] -> Ok(input, Some(value))
            | _ -> failwith "TODO" }

let setCombinator: Combinator =
    { Name = Name("set")
      Doc = "Set the value of a given Network."
      Eval =
        fun _ ((selected, networks): State) arguments ->
            match arguments with
            | [ LigatureValue.NetworkName(name); LigatureValue.Network(value) ] ->
                let newNetworks = Map.add name value networks
                Ok((selected, newNetworks), None)
            | _ -> failwith "TODO" }

let ignoreCombinator: Combinator =
    { Name = Name("ignore")
      Doc = "Ignore any arguments passed and return working state unchanged."
      Eval = fun _ (input: State) _ -> Ok(input, None) }

let docsCombinator: Combinator =
    { Name = Name("docs")
      Doc = "Create a network that contains documentation for the available combinators."
      Eval =
        fun combinators (input: State) _ ->
            let mutable docs = emptyNetwork

            Map.toList combinators
            |> List.iter (fun (name, combinator) ->
                docs <-
                    Set.add
                        (PatternName.Name(name), PatternName.Name(Name("")), LigatureValue.String(combinator.Doc))
                        docs)

            printfn $"{Ligature.Wander.Model.printNetwork docs}"
            Ok(input, Some(LigatureValue.Network docs)) }


let coreCombinators =
    (Map.ofList
        [ (docsCombinator.Name, docsCombinator)
          (idCombinator.Name, idCombinator)
          (ignoreCombinator.Name, ignoreCombinator)
          (setCombinator.Name, setCombinator) ])
