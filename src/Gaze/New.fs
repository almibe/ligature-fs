// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module New

type State<'input> =
    { input: 'input array
      offset: int }

type GazeError = | NoMatch

type Nibbler<'input, 'output> = 
    State<'input> -> Result<(State<'input> * 'output), GazeError>

type Gaze<'input, 'output> = 
    State<'input> -> Result<'output, GazeError>

// let chain<'input, 'output> 
//     (result: Result<(Source<'input> * 'output), GazeError>)
//     (nibbler: Nibbler<'input, 'output>): Result<(Source<'input> * 'output), GazeError> =
//     match result with
//     | Ok(res) ->
//         nibbler res
//     | Error err -> Error err

let explode (s: string) = [| for c in s -> c |]

/// Create an instance of Gaze that works with a String as input.
let fromString string =
    { input = explode (string)
      offset = 0 }

let fromArray array = { input = array; offset = 0 }

let fromList list =
    { input = List.toArray list
      offset = 0 }

let isComplete (gaze: State<_>) =
    gaze.offset >= Array.length (gaze.input)

let peek (gaze: State<_>) : Result<_, GazeError> =
    if isComplete gaze then
        Error(NoMatch)
    else
        Ok(gaze.input[gaze.offset])

let next (gaze: State<_>) : Result<State<_> * _, GazeError> =
    if isComplete (gaze) then
        Error(NoMatch)
    else
        let result = gaze.input[gaze.offset]
        Ok({gaze with offset = gaze.offset + 1}, result)

let read<'i> (number: int) (state: State<'i>) =
    let mutable index = 0
    let result = [| for i in 1 .. number -> None |]
    while index < number do
        if state.offset + index < state.input.Length then
            result[index] <- Some(state.input[state.offset + index])
        index <- index + 1
    result

let readOffset<'i> (number: int) (state: State<'i>) =
    if state.offset + number < state.input.Length then
        Some(state.input[state.offset+number])
    else
        None
