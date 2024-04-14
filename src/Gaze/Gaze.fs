// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Gaze

type Gaze<'input> =
    { content: 'input array
      mutable offset: int }

type GazeError = | NoMatch

type Nibbler<'input, 'output> = Gaze<'input> -> Result<'output, GazeError>

let explode (s: string) = [| for c in s -> c |]

/// Create an instance of Gaze that works with a String as input.
let fromString string =
    { content = explode (string)
      offset = 0 }

let fromArray array = { content = array; offset = 0 }

let fromList list =
    { content = List.toArray list
      offset = 0 }

let isComplete (gaze: Gaze<_>) =
    gaze.offset >= Array.length (gaze.content)

let peek (gaze: Gaze<_>) : Result<_, GazeError> =
    if isComplete gaze then
        Error(NoMatch)
    else
        Ok(gaze.content[gaze.offset])

let next (gaze: Gaze<_>) : Result<_, GazeError> =
    if isComplete (gaze) then
        Error(NoMatch)
    else
        let result = gaze.content[gaze.offset]
        gaze.offset <- gaze.offset + 1
        Ok(result)

let check (nibbler: Nibbler<_, _>) (gaze: Gaze<_>) =
    let startOffset = gaze.offset
    let res = nibbler gaze
    gaze.offset <- startOffset
    res

let attempt (nibbler: Nibbler<_, _>) (gaze: Gaze<_>) =
    let startOffset = gaze.offset

    match nibbler (gaze: Gaze<_>) with
    | Ok(res) -> Ok(res)
    | Error(err) ->
        gaze.offset <- startOffset
        Error(err)

let offset (gaze: Gaze<_>) = gaze.offset

let map (nibbler: Nibbler<_, _>) mapper (gaze: Gaze<_>) =
    match attempt nibbler gaze with
    | Ok(result) -> Ok(mapper (result))
    | Error(err) -> Error(err)

let remaining (gaze: Gaze<_>) = gaze.content[gaze.offset]
