// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Lib

open Ligature.Wander.Model
open Ligature.Main
open Ligature.Wander.Main
open Ligature.Wander.Interpreter

// let writeValueFunction =
//     { Name = "writeValue"
//       Returns = WanderType.String
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ value ] -> Ok(WanderValue.String(prettyPrint value))
//             | value -> error $"Unexpected value - {value}." None) }

// let readValueFunction =
//     { Name = "readValue"
//       Returns = WanderType.Value
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ WanderValue.String(input) ] -> run input (newBindings ())
//             | value -> error $"Unexpected value - {value}." None) }

let idCombinator: Combinator =
    { Name = "id"
      Eval = fun (input: State) -> Ok input }

let clearCombinator =
    { Name = "clear"
      Eval = fun ((networkName, networks): State) -> Ok(networkName, (Map.remove networkName networks)) }

let unionCombinator =
  { Name = "union"
    Eval = fun (inputState: State) -> 
      let networkName, networks = inputState
      let currentNetwork = currentNetwork inputState
      let left = readBinding (PatternWord.Word(Word("left"))) currentNetwork
      let right = readBinding (PatternWord.Word(Word("right"))) currentNetwork
      let out = readBinding (PatternWord.Word(Word("out"))) currentNetwork
      printfn $"{(left, right, out)}"
      match (left, right, out) with
      | (Some(LigatureValue.NetworkName(left)), Some(LigatureValue.NetworkName(right)), Some(LigatureValue.NetworkName(out))) ->
        let leftNetwork = readNetwork left inputState
        let rightNetwork = readNetwork right inputState
        let outNetwork = readNetwork out inputState
        let res = Set.union leftNetwork rightNetwork |> Set.union outNetwork
        printfn $"{networkName}"
        Ok(networkName, Map.add out res networks)
      | _ -> failwith "TODO" }

let applyCombinator: Combinator =
    { Name = "apply"
      Eval =
        fun (inputState: State) ->
            let currentNetwork = currentNetwork inputState
            let data = readBinding (PatternWord.Word(Word("data"))) currentNetwork
            let template = readBinding (PatternWord.Word(Word("template"))) currentNetwork
            let out = readBinding (PatternWord.Word(Word("out"))) currentNetwork

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
                [ (PatternWord.Word(Word("id")), PatternWord.Word(Word("=")), LigatureValue.HostCombinator idCombinator)
                  (PatternWord.Word(Word("union")), PatternWord.Word(Word("=")), LigatureValue.HostCombinator unionCombinator)
                  (PatternWord.Word(Word("clear")),
                   PatternWord.Word(Word("=")),
                   LigatureValue.HostCombinator clearCombinator)
                  (PatternWord.Word(Word("apply")),
                   PatternWord.Word(Word("=")),
                   LigatureValue.HostCombinator applyCombinator) ]) ]
     ))

// let stdLib: Map<string, Word> =
//     Map
//         [
//         //     ("pop",
//         //    { Eval =
//         //        fun words stack ->
//         //            match stack with
//         //            | [] -> Ok([]) //TODO maybe have this be an error?
//         //            | [ _ ] -> Ok([])
//         //            | _ :: tail -> Ok(tail) })
//         //   ("dup",
//         //    { Eval =
//         //        fun words stack ->
//         //            match stack with
//         //            | [] -> Ok([]) //TODO maybe have this be an error?
//         //            | [ head ] -> Ok([ head; head ])
//         //            | head :: tail -> Ok(head :: head :: tail) })
//         //   ("apply",
//         //    { Eval =
//         //        fun words stack ->
//         //            match stack |> List.tryHead with
//         //            | None -> Ok([]) //TODO maybe have this be an error?
//         //            | Some(WanderValue.Quote(head)) ->
//         //                match evalValues words (stack.Tail) head with
//         //                | Ok(res) -> Ok(res @ (List.tail stack))
//         //                | Error(err) -> failwith "TODO"
//         //            | Some(_) -> failwith "TODO" })
//         ]
