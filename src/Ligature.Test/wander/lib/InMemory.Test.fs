// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Keylime.Test

open Expecto
open Ligature.Wander.Lib.Keylime

let createInstance () =
//    new KeylimeLMDB(new LightningEnvironment("C:\\Users\\berry3\\.ligature\\test"))
    KeylimeInMemory.empty ()

[<Tests>]
let tests =
    testList
        "Keylime Test"
        [ testCase ""
          <| fun _ ->
            let keylime = createInstance()
            Expect.equal (keylime.stores ()) (Seq.empty) ""
        //   testCase "Read Integer Token"
        //   <| fun _ ->
        //       Expect.equal (tokenize "123") (Ok([ Token.Int(123I) ])) ""
        //       Expect.equal (tokenize "0") (Ok([ Token.Int(0I) ])) ""
        //       Expect.equal (tokenize "-4123") (Ok([ Token.Int(-4123I) ])) "" 
        ]
