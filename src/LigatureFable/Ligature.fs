// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Fable

type ScriptResult =
    | String of string
    | Integer of bigint
    | Nothing
    | Identifier of string
    | Boolean of bool
    | Array of ScriptResult seq
    | Graph

let readScriptResult (input: string) = failwith ""
//TODO read nothing
//TODO read integer
//TODO read string
//TODO read identifier
//TODO read boolean
//TODO read list
//TODO read array

// module Ligature.Wander.Fable

// open Ligature.Wander.Main
// open Ligature.Wander.Bindings
// open Lexer

// let execute script = run script (newBindings())

// let introspect script = introspect script

// /// Highlight returns an array of arrays that contains the class of the html element and the text of the html element.
// let highlight script =
//     match (introspect script).tokens with
//     | Ok(tokens) ->
//         List.map (fun token ->
//             match token with
//             | Token.Nothing -> ["nothing"; "nothing"]
//             | Token.Bool(bool) -> ["bool"; string bool]
//             | Token.WhiteSpace(ws) -> ["ws"; ws]
//             | Token.Identifier(id) -> ["identifier"; id.ToString ()]
//             | Token.Int(int) -> ["integer"; string int]
//             | Token.Comment(comment) -> ["comment"; comment]
//             | Token.NewLine(nl) -> ["newline"; nl]
//             | Token.StringLiteral(stringLiteral) -> ["string"; stringLiteral]
//             | Token.BytesLiteral(_) -> failwith "Not Implemented"
//             | Token.Name(name) -> ["name"; name]
//             | Token.OpenBrace -> ["openBrace"; "{"]
//             | Token.CloseBrace -> ["closeBrace"; "}"]
//             | Token.Colon -> ["colon"; ":"]
//             | Token.OpenParen -> ["openParen"; "("]
//             | Token.CloseParen -> ["closeParen"; ")"]
//             | Token.OpenSquare -> ["openSquare"; "["]
//             | Token.CloseSquare -> ["closeSquare"; "]"]
//             | Token.Arrow -> ["arrow"; "->"]
//             | Token.WideArrow -> ["wideArrow"; "=>"]
//             | Token.EqualsSign -> ["equalsSign"; "="]
//             | Token.Dot -> ["dot"; "."]
//             | Token.QuestionMark -> ["questionMark"; "?"]
//             | Token.WhenKeyword -> ["whenKeyword"; "when"]
//             | Token.Hash -> ["hash"; "#"]
//             | Token.Lambda -> ["lambda"; "\\"]
//             | Token.Comma -> ["comma"; ","]) tokens
//     | Error(e) -> [["error"; $"Error {e}"]]
