// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Model

open Ligature.Model

type ExstRestriction = { role: Term; filler: Term }

[<RequireQualifiedAccess>]
type ConceptExp =
    | ConceptName of Term
    | ConceptNegation of Term
    | ConceptConjection of Set<ConceptExp>
    | ExstRestriction of ExstRestriction

[<RequireQualifiedAccess>]
type ConceptDef =
    | ConceptEquiv of Term * ConceptExp
    | ConceptSub of Term * ConceptExp

let networkToModel (network: Network) : Set<ConceptDef> =
    Set.filter (fun triple -> 
        match triple with
        | _, Term "tiny-dl.≡", _ -> true
        | _ -> false) network
    |> Set.map (fun triple ->
        match triple with
        | concept, Term "tiny-dl.≡", Value.Term equiv ->
            ConceptDef.ConceptEquiv (concept, ConceptExp.ConceptName equiv)
        | _ -> failwith "TODO")
