// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Preludes
open Ligature.Bend
open Bool
open Ligature

let bindStandardLibrary bindings =
    bindings
    |> Bindings.bind "Bool" boolLib

let standardPrelude () =
    bindStandardLibrary (Bindings.newBindings ())

let instancePrelude instance = 
    standardPrelude ()
    |> Bindings.bind "Ligature" (ligatureLib instance)
