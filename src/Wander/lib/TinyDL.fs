// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Lib.TinyDL

open Ligature.Main


let inferCombinator: Combinator =
    { Name = Symbol("infer")
      Doc = "Use the ."
      Signature = [ LigatureType.Network; LigatureType.Network ], None
      Eval =
        fun _ store arguments ->
            match arguments with
            | [ WanderValue.Network(tBox); WanderValue.Network(aBox) ] ->
                failwith "TODO"
                //infer tBox aBox
            | _ -> error "Improper call to infer." None }

let parseCombinator: Combinator = 
    { Name = Symbol "tiny-dl.parse"
      Doc = "Parse tiny-dl script into a Network."
      Signature = [ LigatureType.Symbol ], None
      Eval =
        fun _ store arguments ->
            match arguments with
            | [ WanderValue.Symbol(input) ] -> 
//                TinyDL.
                failwith "TODO" }

let tinyDLCombinators =
    (Map.ofList
        [ (inferCombinator.Name, inferCombinator) ])
