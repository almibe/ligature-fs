// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module NibblersSuite

open Expecto

[<Tests>]
let tests =
    testList
        "Nibbler Tests"
        [ testList
              "Take Suite"
              [ testCase "take with single value"
                <| fun _ ->
                    let gaze = Gaze.fromString ("a")
                    Expect.equal (Gaze.attempt (Nibblers.take 'a') gaze) (Ok('a')) ""
                    Expect.isTrue (Gaze.isComplete gaze) "" ]

          testList
              "Take List Suite"
              [ testCase "takeList simple"
                <| fun _ ->
                    let gaze = Gaze.fromString ("hello, world")
                    let list = [ 'h'; 'e'; 'l'; 'l'; 'o' ]
                    Expect.equal (Gaze.attempt (Nibblers.takeList list) gaze) (Ok(list)) ""
                    Expect.equal (Gaze.peek gaze) (Ok(',')) "" ]

          testList
              "Take String Suite"
              [ testCase "takeString simple"
                <| fun _ ->
                    let gaze = Gaze.fromString ("hello, world")

                    Expect.equal
                        (Gaze.attempt (Nibblers.takeString "hello") gaze)
                        (Ok([ 'h'; 'e'; 'l'; 'l'; 'o' ]))
                        ""

                    Expect.equal (Gaze.peek gaze) (Ok(',')) "" ]

          testList
              "Take Cond Suite"
              [ testCase "takeCond with a single value"
                <| fun _ ->
                    let gaze = Gaze.fromString ("a")
                    Expect.equal (Gaze.attempt (Nibblers.takeCond (fun c -> c = 'a')) gaze) (Ok('a')) ""
                    assert (Gaze.isComplete (gaze))
                testCase "takeCond with multiple values"
                <| fun _ ->
                    let gaze = Gaze.fromString ("cab")

                    Expect.equal
                        (Gaze.attempt (Nibblers.takeCond (fun c -> List.contains c [ 'a'; 'b'; 'c'; 'd' ])) gaze)
                        (Ok('c'))
                        ""

                    Expect.isFalse (Gaze.isComplete gaze) ""
                testCase "takeCond match beginning"
                <| fun _ ->
                    let gaze = Gaze.fromString ("abc123")

                    Expect.equal
                        (Gaze.attempt (Nibblers.takeCond (fun c -> List.contains c [ 'a'; 'b'; 'c'; 'd' ])) gaze)
                        (Ok('a'))
                        ""

                    Expect.isFalse (Gaze.isComplete gaze) ""
                testCase "takeCond no match beginning"
                <| fun _ ->
                    let gaze = Gaze.fromString ("123abc")

                    Expect.equal
                        (Gaze.attempt (Nibblers.takeCond (fun c -> List.contains c [ 'a'; 'b'; 'c'; 'd' ])) gaze)
                        (Error Gaze.GazeError.NoMatch)
                        ""

                    Expect.isFalse (Gaze.isComplete gaze) "" ]

          testList
              "Take In Range Suite"
              [ testCase "basic range test"
                <| fun _ ->
                    let gaze = Gaze.fromArray [| 1; 2; 3 |]
                    Expect.equal (Gaze.attempt (Nibblers.takeInRange [ (1, 10) ]) gaze) (Ok(1)) ""
                    Expect.equal (Gaze.attempt (Nibblers.takeInRange [ (5, 10) ]) gaze) (Error Gaze.GazeError.NoMatch) ""
                    Expect.equal (Gaze.attempt (Nibblers.takeInRange [ (1, 10) ]) gaze) (Ok(2)) ""
                    Expect.equal (Gaze.attempt (Nibblers.takeInRange [ (5, 10); (-1, 1); (0, 1) ]) gaze) (Error Gaze.GazeError.NoMatch) ""
                    Expect.equal (Gaze.attempt (Nibblers.takeInRange [ (5, 10); (-1, 1); (2, 3) ]) gaze) (Ok(3)) "" ]

          testList
              "Take While Suite"
              [ testCase "takeWhile with a single value"
                <| fun _ ->
                    let gaze = Gaze.fromString ("a")
                    Expect.equal (Gaze.attempt (Nibblers.takeWhile (fun c -> c = 'a')) gaze) (Ok([ 'a' ])) ""
                    Expect.isTrue (Gaze.isComplete gaze) ""
                testCase "takeWhile with multiple values"
                <| fun _ ->
                    let gaze = Gaze.fromString ("cab")

                    Expect.equal
                        (Gaze.attempt (Nibblers.takeWhile (fun c -> List.contains c [ 'a'; 'b'; 'c'; 'd' ])) gaze)
                        (Ok([ 'c'; 'a'; 'b' ]))
                        ""

                    Expect.isTrue (Gaze.isComplete gaze) ""
                testCase "takeWhile match beginning"
                <| fun _ ->
                    let gaze = Gaze.fromString ("abc123")

                    Expect.equal
                        (Gaze.attempt (Nibblers.takeWhile (fun c -> List.contains c [ 'a'; 'b'; 'c'; 'd' ])) gaze)
                        (Ok([ 'a'; 'b'; 'c' ]))
                        ""

                    Expect.isFalse (Gaze.isComplete gaze) ""
                testCase "takeWhile no match beginning"
                <| fun _ ->
                    let gaze = Gaze.fromString ("123abc")

                    Expect.equal
                        (Gaze.attempt (Nibblers.takeWhile (fun c -> List.contains c [ 'a'; 'b'; 'c'; 'd' ])) gaze)
                        (Error Gaze.GazeError.NoMatch)
                        ""

                    Expect.isFalse (Gaze.isComplete gaze) "" ]

          testList
              "Take While Index Suite"
              [ testCase "takeWhileIndex simple"
                <| fun _ ->
                    let gaze = Gaze.fromString ("abc")

                    Expect.equal
                        (Gaze.attempt
                            (Nibblers.takeWhileIndex (fun (c, i) -> (c = 'a' && i = 0) || (c = 'b' && i = 1)))
                            gaze)
                        (Ok([ 'a'; 'b' ]))
                        ""

                    Expect.isFalse (Gaze.isComplete gaze) "" ]

          testList
              "Take Until Suite"
              [ testCase "takeUntil simple"
                <| fun _ ->
                    let gaze = Gaze.fromString ("hello, world")

                    Expect.equal
                        (Gaze.attempt (Nibblers.takeUntil (Nibblers.take ',')) gaze)
                        (Ok([ 'h'; 'e'; 'l'; 'l'; 'o' ]))
                        ""

                    Expect.equal (Gaze.peek gaze) (Ok(',')) "" ]

          testList
              "Between Suite"
              [ testCase "between simple"
                <| fun _ ->
                    let gaze = Gaze.fromString ("abbbbbc")

                    Expect.equal
                        (Gaze.attempt (Nibblers.between 'a' (Nibblers.takeWhile (fun c -> c = 'b')) 'c') gaze)
                        (Ok([ 'b'; 'b'; 'b'; 'b'; 'b' ]))
                        ""

                    Expect.isTrue (Gaze.isComplete gaze) "" ]

          testList
              "Optional Suite"
              [ testCase "optional simple"
                <| fun _ ->
                    let gaze = Gaze.fromString ("a")

                    Expect.equal
                        (Gaze.attempt (Nibblers.optional (Gaze.map (Nibblers.take 'b') (fun r -> [ r ]))) gaze)
                        (Ok([]))
                        ""

                    Expect.equal (Gaze.peek gaze) (Ok('a')) "" ]

          testList
              "Repeat Suite"
              [ testCase "repeat simple"
                <| fun _ ->
                    let gaze = Gaze.fromString ("aaaabbbbbc")

                    Expect.equal
                        (Gaze.attempt (Nibblers.repeat (Nibblers.takeCond (fun c -> c = 'a'))) gaze)
                        (Ok([ 'a'; 'a'; 'a'; 'a' ]))
                        ""

                    Expect.isFalse (Gaze.isComplete gaze) ""

                    Expect.equal
                        (Gaze.attempt (Nibblers.repeat (Nibblers.takeCond (fun c -> c = 'b'))) gaze)
                        (Ok([ 'b'; 'b'; 'b'; 'b'; 'b' ]))
                        ""

                    Expect.isFalse (Gaze.isComplete gaze) ""

                    Expect.equal
                        (Gaze.attempt (Nibblers.repeat (Nibblers.takeCond (fun c -> c = 'c'))) gaze)
                        (Ok([ 'c' ]))
                        ""

                    Expect.isTrue (Gaze.isComplete gaze) "" ]

          testList
              "RepeatSep Suite"
              [ testCase "repeatSep simple"
                <| fun _ ->
                    let gaze = Gaze.fromString ("a,a,b,b,c,")

                    Expect.equal
                        (Gaze.attempt (Nibblers.repeatSep (Nibblers.takeCond (fun c -> c = 'a')) ',') gaze)
                        (Ok([ 'a'; 'a'; ]))
                        ""

                    Expect.isFalse (Gaze.isComplete gaze) ""

                    Expect.equal
                        (Gaze.attempt (Nibblers.repeatSep (Nibblers.takeCond (fun c -> c = 'b')) ',') gaze)
                        (Ok([ 'b'; 'b'; ]))
                        ""

                    Expect.isFalse (Gaze.isComplete gaze) ""

                    Expect.equal
                        (Gaze.attempt (Nibblers.repeatSep (Nibblers.takeCond (fun c -> c = 'c')) ',') gaze)
                        (Ok([ 'c' ]))
                        ""

                    Expect.isTrue (Gaze.isComplete gaze) "" ]

          testList
              "Take All Suite"
              [ testCase "take all simple"
                <| fun _ ->
                    let gaze = Gaze.fromString ("aaaabbbbbc")
                    let takeAs = Nibblers.repeat (Nibblers.takeCond (fun c -> c = 'a'))
                    let takeBs = Nibblers.repeat (Nibblers.takeCond (fun c -> c = 'b'))
                    let takeCs = Nibblers.repeat (Nibblers.takeCond (fun c -> c = 'c'))
                    let takeAll = Nibblers.takeAll [ takeAs; takeBs; takeCs ]

                    Expect.equal
                        (Gaze.attempt takeAll gaze)
                        (Ok([ [ 'a'; 'a'; 'a'; 'a' ]; [ 'b'; 'b'; 'b'; 'b'; 'b' ]; [ 'c' ] ]))
                        ""

                    Expect.isTrue (Gaze.isComplete gaze) "" ]

          testList
              "Take First Suite"
              [ testCase "take first simple"
                <| fun _ ->
                    let gaze = Gaze.fromString ("aaaabbbbbc")
                    let takeAs = Nibblers.repeat (Nibblers.takeCond (fun c -> c = 'a'))
                    let takeBs = Nibblers.repeat (Nibblers.takeCond (fun c -> c = 'b'))
                    let takeCs = Nibblers.repeat (Nibblers.takeCond (fun c -> c = 'c'))
                    let takeFirst = Nibblers.takeFirst [ takeCs; takeBs; takeAs ]
                    Expect.equal (Gaze.attempt takeFirst gaze) (Ok([ 'a'; 'a'; 'a'; 'a' ])) ""
                    Expect.isFalse (Gaze.isComplete gaze) "" ] ]
