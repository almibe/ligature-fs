// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Parser.Test

open Expecto
open Ligature.Wander.Parser
open Ligature.Wander.Lexer
open Ligature.Wander.Model

[<Tests>]
let tests =
    testList "Parser Test" [
        testCase "Parse Integer" <| fun _ ->
            let tokens = [Ligature.Wander.Lexer.WanderToken.Integer(345)]
            let ast = parse tokens
            Expect.equal ast (Ok([Ligature.Wander.Model.Expression.Value(WanderValue.Integer(345))])) ""
    ]
