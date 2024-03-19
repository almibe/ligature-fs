// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Bindings

type Scope<'K, 'V when 'K: comparison> = Map<'K, 'V>

type Bindings<'K, 'V when 'K: comparison> = { current: Scope<'K, 'V>; stack: Scope<'K, 'V> list }

let newBindings () = { current = Map<'K, 'V> []; stack = [] }

let bind name value bindings =
    let current' = Map.add name value bindings.current
    { bindings with current = current' }

let addScope bindings =
    let current = Map []
    let stack = List.append [ bindings.current ] bindings.stack
    { current = current; stack = stack }

let removeScope bindings =
    let current = List.head bindings.stack
    let stack = List.tail bindings.stack
    { current = current; stack = stack }

let rec read name bindings =
    if Map.containsKey name bindings.current then
        Some(Map.find name bindings.current)
    else if List.isEmpty bindings.stack then
        None
    else
        read name (removeScope bindings)
