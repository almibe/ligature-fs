// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Preludes

open Ligature.Wander
open Bool
open Bytes
open Identifier
open Ligature
open Statement
open Int
open Wander
open DateTime

let bindStandardLibrary bindings =
    bindings
    |> Bindings.bind "Array" Array.arrayLib
    |> Bindings.bind "Assert" Assert.assertLib
    |> Bindings.bind "Bool" boolLib
    |> Bindings.bind "Bytes" bytesLib
    |> Bindings.bind "DateTime" dateTimeLib
    |> Bindings.bind "Dataset" Dataset.datasetLib
    |> Bindings.bind "Identifier" identifierLib
    |> Bindings.bind "Int" intLib
    |> Bindings.bind "Statement" statementLib
    |> Bindings.bind "String" String.stringLib
    |> Bindings.bind "Wander" wanderLib

let standardPrelude () =
    bindStandardLibrary (Bindings.newBindings ())

let instancePrelude instance =
    standardPrelude () |> Bindings.bind "Ligature" (ligatureLib instance)
