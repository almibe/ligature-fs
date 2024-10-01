// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Lib.TinyDL

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
            | [ WanderValue.Symbol(Symbol(name)); WanderValue.Network(value) ] ->
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
            | [ WanderValue.Symbol(Symbol(name)) ] ->
                let network = WanderValue.Network(store.Read name)
                Ok(Some(network))
            // match Map.tryFind name networks with
            // | Some(network) -> Ok(selected, networks, Some(Identifier.Network network))
            // | _ -> failwith "TODO"
            | _ -> failwith "TODO" }

let tinyDLCombinators =
    (Map.ofList
        [ (idCombinator.Name, idCombinator)
          (readCombinator.Name, readCombinator)
          (setCombinator.Name, setCombinator) ])
