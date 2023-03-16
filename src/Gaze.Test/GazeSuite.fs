// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module GazeSuite

open Expecto

[<Tests>]
let tests =
    testList
        "Gaze Suite"
        [ testCase "empty String input"
          <| fun _ ->
              let gaze = Gaze.fromString ("")
              Expect.isTrue (Gaze.isComplete gaze) ""
              Expect.equal (Gaze.peek gaze) None ""
              Expect.equal (Gaze.peek gaze) None ""
              Expect.isTrue (Gaze.isComplete gaze) ""

          testCase "empty array input"
          <| fun _ ->
              let gaze = Gaze.fromArray ([||])
              Expect.isTrue (Gaze.isComplete gaze) ""
              Expect.equal (Gaze.peek gaze) None ""
              Expect.equal (Gaze.peek gaze) None ""
              Expect.isTrue (Gaze.isComplete gaze) ""

          testCase "init Gaze with one value"
          <| fun _ ->
              let gaze = Gaze.fromArray ([| 'a' |])
              Expect.isFalse (Gaze.isComplete gaze) ""
              Expect.equal (Gaze.peek gaze) (Some('a')) ""
              Expect.equal (Gaze.peek gaze) (Some('a')) ""
              Expect.equal (Gaze.next gaze) (Some('a')) ""
              Expect.equal (Gaze.next gaze) None ""
              Expect.isTrue (Gaze.isComplete gaze) ""

          testCase "init Gaze with single char String"
          <| fun _ ->
              let gaze = Gaze.fromString ("a")
              Expect.isFalse (Gaze.isComplete gaze) ""
              Expect.equal (Gaze.peek gaze) (Some('a')) ""
              Expect.equal (Gaze.peek gaze) (Some('a')) ""
              Expect.equal (Gaze.next gaze) (Some('a')) ""
              Expect.equal (Gaze.next gaze) None ""
              Expect.isTrue (Gaze.isComplete gaze) ""

          testCase "map digit"
          <| fun _ ->
              let gaze = Gaze.fromString ("1")
              let result = Some(1)
              let nibbler = Gaze.map Gaze.next (fun char -> int (string char))
              Expect.equal (Gaze.attempt nibbler gaze) result "" ]
