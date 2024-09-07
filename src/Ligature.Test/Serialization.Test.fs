// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Serialization.Test

open Expecto
open Ligature.Main

// let ident id =
//     match identifier id with
//     | Ok id -> id
//     | _ -> failwith "Error"

// let vident id =
//     match identifier id with
//     | Ok id -> Value.Name id
//     | _ -> failwith "Error"

// [<Tests>]
// let tests =
//     testList
//         "Parser Suite"
//         [ testCase "Parse empty Network"
//           <| fun _ ->
//               let result = (Ligature.Serialization.readLigature "{}") |> Result.toOption
//               let expect: Network = emptyNetwork
//               Expect.equal (result.Value.Write()) (expect.Write()) ""
//           //   testCase "Parse simple Network"
//           //   <| fun _ ->
//           //       let result = Ligature.Serialization.readLigature "{`a` `b` `c`}" |> Result.toOption

//           //       let expect: Set<Triple> =
//           //           Set.ofList [ triple (Pattern.Id(ident "a")) (Pattern.Id(ident "b")) (vident "c") ]

//           //       Expect.equal (result.Value.Write()) expect ""
//           testCase "Write Empty Network"
//           <| fun _ ->
//               let result = Ligature.Serialization.writeLigature (InMemoryNetwork(Set.empty))
//               let expect = "{}"
//               Expect.equal result expect "" ]
