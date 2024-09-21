// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module NewSuite

open Expecto

let noMatch = (Error New.GazeError.NoMatch)

[<Tests>]
let tests =
    testList
        "New Suite"
        [ testCase "empty String input"
          <| fun _ ->
              let gaze = New.fromString ("")
              Expect.isTrue (New.isComplete gaze) ""
              Expect.equal (New.peek gaze) noMatch ""
              Expect.equal (New.peek gaze) noMatch ""
              Expect.isTrue (New.isComplete gaze) ""

          testCase "empty array input"
          <| fun _ ->
              let gaze = New.fromArray ([||])
              Expect.isTrue (New.isComplete gaze) ""
              Expect.equal (New.peek gaze) noMatch ""
              Expect.equal (New.peek gaze) noMatch ""
              Expect.isTrue (New.isComplete gaze) ""

          testCase "test read"
          <| fun _ ->
              let gaze = New.fromArray ([| 1; 2; 3 |])
              Expect.isFalse (New.isComplete gaze) ""
              Expect.equal (New.read 2 gaze) ([| Some(1); Some(2) |]) ""
              Expect.equal (New.read 2 { input = [| 1; 2; 3; 4 |]; offset = 2 }) ([| Some(3); Some(4) |]) ""
              Expect.equal (New.read 4 { input = [| 1; 2; 3; 4 |]; offset = 2 }) ([| Some(3); Some(4); None; None |]) ""
              Expect.equal (New.readOffset 2 gaze) (Some(3)) ""
              Expect.equal (New.readOffset 3 gaze) (None) "" ]
