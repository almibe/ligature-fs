// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemoryEngine

open Ligature.Main

let isComplete (entries: Set<Entry>) : bool =
    let concepts =
        Set.fold
            (fun state value ->
                match value with
                | Entry.Extends { concept = concept } -> Set.add concept state
                | _ -> state)
            Set.empty
            entries

    Set.fold
        (fun state (entry: Entry) ->
            match entry with
            //            | Entry.Attribute { first = first; second = second } -> (concepts.Contains first) && (concepts.Contains second)
            | _ -> state)
        true
        entries

let isConsistent (network: Network) : bool =
    let mutable individuals: Map<Element, Set<Entry>> = Map.empty

    Set.fold
        (fun state (entry: Entry) ->
            match state with
            | false -> false
            | true ->
                match entry with
                | Entry.Extends { concept = conceptName
                                  element = symbol } ->
                    let concept =
                        Entry.Extends
                            { concept = conceptName
                              element = symbol }

                    let notVersion =
                        Entry.NotExtends
                            { concept = conceptName
                              element = symbol }

                    match individuals.TryFind symbol with
                    | None ->
                        individuals <- Map.add symbol (Set.ofList [ concept ]) individuals
                        true
                    | Some res ->
                        if res.Contains(notVersion) then
                            false
                        else
                            individuals <- Map.add symbol (Set.add (concept) res) individuals
                            true
                | Entry.NotExtends { concept = concept; element = symbol } ->
                    let notConcept = Entry.NotExtends { concept = concept; element = symbol }
                    let inverse = Entry.Extends { concept = concept; element = symbol }

                    match individuals.TryFind symbol with
                    | None ->
                        individuals <- Map.add symbol (Set.ofList [ notConcept ]) individuals
                        true
                    | Some res ->
                        if res.Contains(inverse) then
                            false
                        else
                            individuals <- Map.add symbol (Set.add notConcept res) individuals
                            true
                | Entry.Attribute _ -> true)
        true
        network
