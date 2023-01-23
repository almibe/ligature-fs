// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Lexer.Test

open Expecto
open Ligature
open Wander.Lexer

[<Tests>]
let tests =
    testList "Lexer Test" [
        testCase "Read Integer Token" <| fun _ ->
            Expect.equal (tokenize "123") (Ok([Integer(123)])) ""
            Expect.equal (tokenize "0") (Ok([Integer(0)])) ""
            Expect.equal (tokenize "-4123") (Ok([Integer(-4123)])) ""
        testCase "Read Names" <| fun _ ->
            Expect.equal (tokenize "hello") (Ok([Name("hello")])) ""
        testCase "tokenize booleans" <| fun _ ->
            Expect.equal (tokenize "true") (Ok([Boolean(true)])) ""
            Expect.equal (tokenize "false")  (Ok([Boolean(false)])) ""
        testCase "tokenize whitespace" <| fun _ ->
            Expect.equal (tokenize " ") (Ok([WhiteSpace(" ")])) ""
            Expect.equal (tokenize "   ") (Ok([WhiteSpace("   ")])) ""
            Expect.equal (tokenize "\t") (Ok([WhiteSpace("\t")])) ""
            Expect.equal (tokenize "\t  ") (Ok([WhiteSpace("\t  ")])) ""
        testCase "tokenize new lines" <| fun _ ->
            Expect.equal (tokenize "\n") (Ok([NewLine("\n")])) ""
            Expect.equal (tokenize "\r\n") (Ok([NewLine("\r\n")])) ""
            Expect.equal (tokenize "\n\n\r\n") (Ok([NewLine("\n\n\r\n")])) ""
    ]
