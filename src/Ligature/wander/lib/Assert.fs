// // This Source Code Form is subject to the terms of the Mozilla Public
// // License, v. 2.0. If a copy of the MPL was not distributed with this
// // file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Assert

open Ligature.Wander.Model
open Ligature.Main
open Ligature.InMemoryNetwork

let equalFunction =
    { Module = "Assert"
      Name = "assertEqual"
      Description = "Check that two valeus are equal or fail."
      Parameters = []
      Returns = WanderType.Nothing
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.String(desc); left; right ] ->
                if (left = right) || (wanderEquals left right) then
                    Ok(WanderValue.Network(emptyNetwork))
                else
                    error $"{prettyPrint left} != {prettyPrint right}" None
            | _ -> error "Invalid call to Assert.equal function." None) }

let failFunction =
    { Module = "Assert"
      Name = "fail"
      Description = "Fail this test run."
      Parameters = []
      Returns = WanderType.Nothing
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.String(message) ] -> error message None
            | _ -> error "Invalid call to Assert.fail function." None) }

let assertLib = [ equalFunction; failFunction ]
