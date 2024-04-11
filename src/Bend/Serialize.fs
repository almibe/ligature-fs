// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Serialize

open Ligature.Bend.Model
open Parser
open Lexer
open Ligature
open System.IO

let write (writer: TextWriter) (statements: Statement seq) =
    Seq.iter 
        (fun statement ->

            failwith "TODO") statements

let loadFromString (content: string): Statement seq =

    failwith "TODO"
