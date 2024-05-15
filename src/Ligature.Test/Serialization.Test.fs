// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Parser.Test

open Expecto
open Ligature.Main
open Ligature.InMemoryNetwork

let ident id =
    match identifier id with
    | Ok id -> id
    | _ -> failwith "Error"

let vident id =
    match identifier id with
    | Ok id -> Value.Identifier id
    | _ -> failwith "Error"

[<Tests>]
let tests =
    testList
        "Parser Suite"
        [ testCase "Parse empty Network"
          <| fun _ ->
              let result = Ligature.Serialization.readLigature "{}"
              let expect: Result<INetwork, LigatureError> = Ok(InMemoryNetwork(Set.empty))
              Expect.equal result expect "" 
          testCase "Parse simple Network"
          <| fun _ ->
              let result = Ligature.Serialization.readLigature "{`a` `b` `c`}"
              let expect: Result<INetwork, LigatureError> = Ok(InMemoryNetwork(Set.ofList [
                    statement (ident "a") (ident "b") (vident "c")
              ]))
              Expect.equal result expect "" 
          testCase "Write Empty Network"
          <| fun _ ->
              let result = Ligature.Serialization.writeLigature (InMemoryNetwork(Set.empty))
              let expect = "{}"
              Expect.equal result expect "" 
        ]
