// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Gaze

type Gaze<'input> = {
    content: 'input array
    mutable offset: int
}

type Nibbler<'input, 'output> = Gaze<'input> -> 'output option

let private explode (s:string) =
        [| for c in s -> c |]

/// Create an instance of Gaze that works with a String as input. 
let fromString string = {
    content = explode(string)
    offset = 0
}

let fromArray array = {
    content = array
    offset = 0
}

let isComplete gaze = gaze.offset >= Array.length(gaze.content)

let peek gaze = 
    if isComplete gaze then
        None
    else
        Some(gaze.content[gaze.offset])

let next gaze = 
    if isComplete(gaze) then
        None
    else
        let result = gaze.content[gaze.offset]
        gaze.offset <- gaze.offset + 1
        Some(result)

let check nibbler gaze =
    let startOffset = gaze.offset
    let res = nibbler gaze
    gaze.offset <- startOffset
    res

let attempt nibbler gaze =
    let startOffset = gaze.offset
    match nibbler gaze with
        | Some(res) -> Some(res)
        | None ->
            gaze.offset <- startOffset
            None

let offset gaze = gaze.offset

let map nibbler mapper gaze =
    match attempt nibbler gaze with
        | Some(result) ->
            Some(mapper(result))
        | None -> None
