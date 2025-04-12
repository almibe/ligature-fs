// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Core

open Ligature.Model
open TinyDL.Model

let rec infer (tBox: Definitions) (aBox: Network) : Result<Network, LigatureError> =
    let mutable res = aBox

    // Set.iter
    //     (fun tStatement ->
    //         Set.iter
    //             (fun aStatement ->
    //                 match tStatement, aStatement with
    //                 | (subconcept, Term "subconcept-of", superconcept), (element, Term ":", Value.Term concept) when
    //                     subconcept = concept
    //                     ->
    //                     res <- Set.add (element, Term ":", superconcept) res
    //                 | (firstRole, Term "tiny-dl.inverse-of", Value.Term secondRole), (first, role, Value.Term second) when
    //                     role = firstRole
    //                     ->
    //                     res <- Set.add (second, secondRole, Value.Term first) res
    //                 | (firstRole, Term "tiny-dl.inverse-of", Value.Term secondRole), (first, role, Value.Term second) when
    //                     role = secondRole
    //                     ->
    //                     res <- Set.add (second, firstRole, Value.Term first) res
    //                 | (roleName, Term ":", Value.Term(Term "tiny-dl.Is-Symmetrical")),
    //                   (first, role, Value.Term second) when role = roleName ->
    //                     res <- Set.add (second, role, Value.Term first) res
    //                 | _ -> ())
    //             aBox)
    //     tBox

    if aBox = res then Ok res else infer tBox res
