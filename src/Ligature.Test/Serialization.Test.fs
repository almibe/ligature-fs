// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Parser.Test

open Expecto
open Ligature.Main

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
              let expect: Result<INetwork, LigatureError> = Ok(InMemoryNetwork(Set.empty))
              Expect.equal result expect "" ]
