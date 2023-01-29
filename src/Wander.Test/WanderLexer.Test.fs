// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Lexer.Test

open Expecto
open Ligature
open Wander.Lexer

let ident id = Identifier(match identifier id with | Ok(v) -> v | Error(_) -> todo)

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
            //Expect.equal (tokenize "\t") (Ok([WhiteSpace("\t")])) ""
            //Expect.equal (tokenize "\t  ") (Ok([WhiteSpace("\t  ")])) ""
        testCase "tokenize new lines" <| fun _ ->
            Expect.equal (tokenize "\n") (Ok([NewLine("\n")])) ""
            Expect.equal (tokenize "\r\n") (Ok([NewLine("\r\n")])) ""
            Expect.equal (tokenize "\r\n\r\n\r\n\n") (Ok([NewLine("\r\n\r\n\r\n\n")])) ""
        testCase "Read Identifiers" <| fun _ ->
            Expect.equal (tokenize "<a>") (Ok([(ident "a")])) ""
            Expect.equal (tokenize "<https://ligature.dev/#>") (Ok([ident "https://ligature.dev/#"])) ""
        testCase "Read comments" <| fun _ ->
            Expect.equal (tokenize "--") (Ok([Comment("--")])) ""
            Expect.equal (tokenize "--hello") (Ok([Comment("--hello")])) ""
            Expect.equal (tokenize "-- this is a@#$@%$#@$%@ comment;;;;  ") (Ok([Comment("-- this is a@#$@%$#@$%@ comment;;;;  ")])) ""
            //TODO add test with multiple comments separated by \n
        testCase "read String Literal" <| fun _ ->
            Expect.equal (tokenize @"""hello""") (Ok([StringLiteral("hello")])) ""
        // testCase "read Bytes Literal" <| fun _ ->
        //     Expect.equal (tokenize "0x55") (Ok([Bytes("0x55")])) ""
        testCase "read let keyword" <| fun _ ->
            Expect.equal (tokenize "let") (Ok([LetKeyword])) ""
        testCase "read equals sign" <| fun _ ->
            Expect.equal (tokenize "=") (Ok([EqualSign])) ""
        testCase "read braces" <| fun _ ->
            Expect.equal (tokenize "{") (Ok([OpenBrace])) ""
            Expect.equal (tokenize "}") (Ok([CloseBrace])) ""
            Expect.equal (tokenize "{{}}}") (Ok([OpenBrace; OpenBrace; CloseBrace; CloseBrace; CloseBrace])) ""
        testCase "read colon" <| fun _ ->
            Expect.equal (tokenize ":") (Ok([Colon])) ""
            Expect.equal (tokenize "::::") (Ok([Colon; Colon; Colon; Colon])) ""
        testCase "read dot" <| fun _ ->
            Expect.equal (tokenize ".") (Ok([Dot])) ""
            Expect.equal (tokenize "....") (Ok([Dot; Dot; Dot; Dot])) ""
        testCase "read parens" <| fun _ ->
            Expect.equal (tokenize "(") (Ok([OpenParen])) ""
            Expect.equal (tokenize ")") (Ok([CloseParen])) ""
            Expect.equal (tokenize "(()))") (Ok([OpenParen; OpenParen; CloseParen; CloseParen; CloseParen])) ""
        testCase "read square brackets" <| fun _ ->
            Expect.equal (tokenize "[") (Ok([OpenSquare])) ""
            Expect.equal (tokenize "]") (Ok([CloseSquare])) ""
            Expect.equal (tokenize "[[]]]") (Ok([OpenSquare; OpenSquare; CloseSquare; CloseSquare; CloseSquare])) ""
        testCase "read arrow" <| fun _ ->
            Expect.equal (tokenize "->") (Ok([Arrow])) ""
            Expect.equal (tokenize "->->") (Ok([Arrow; Arrow])) ""
            Expect.equal (tokenize "->->->") (Ok([Arrow; Arrow; Arrow])) ""
        testCase "read if and else keywords" <| fun _ ->
            Expect.equal (tokenize "if") (Ok([IfKeyword])) ""
            Expect.equal (tokenize "elsif") (Ok([ElsifKeyword])) ""
            Expect.equal (tokenize "else") (Ok([ElseKeyword])) ""
        testCase "read question mark" <| fun _ ->
            Expect.equal (tokenize "?") (Ok([QuestionMark])) ""
    ]