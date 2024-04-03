// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Preludes

open Ligature.Bend
open Bool
open Bytes
open Identifier
open Ligature
open Statement
open Test
open Int
open Record
open Bend

let bindStandardLibrary bindings =
    bindings
    |> Bindings.bind "Array" Array.arrayLib
    |> Bindings.bind "Bend" bendLib
    |> Bindings.bind "Bool" boolLib
    |> Bindings.bind "Bytes" bytesLib
    |> Bindings.bind "Identifier" identifierLib
    |> Bindings.bind "Int" intLib
    |> Bindings.bind "Record" recordLib
    |> Bindings.bind "Statement" statementLib
    |> Bindings.bind "String" String.stringLib
    |> Bindings.bind "Test" testLib

let standardPrelude () =
    bindStandardLibrary (Bindings.newBindings ())

let instancePrelude instance = 
    standardPrelude ()
    |> Bindings.bind "Ligature" (ligatureLib instance)
