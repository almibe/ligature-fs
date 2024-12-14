// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemoryNetwork.Test

open Expecto
open Ligature.Model
open Ligature.Core

[<Tests>]
let tests =
    testList
        "Core Test Suite"
        [ testCase "empty filter"
          <| fun _ ->
              let result = filter Set.empty Set.empty
              Expect.equal result Set.empty ""
          testCase "empty pattern"
          <| fun _ ->
              let result =
                  filter
                      Set.empty
                      (Set.ofList
                          [ { element = Element "e"
                              attribute = Element "a"
                              value = Value.Element(Element "v") } ])

              Expect.equal result Set.empty ""
          testCase "empty network"
          <| fun _ ->
              let result =
                  filter
                      (Set.ofList
                          [ { element = ElementPattern.Element(Element "e")
                              attribute = ElementPattern.Element(Element "c")
                              value = ValuePattern.Element(Element "e") } ])
                      Set.empty

              Expect.equal result Set.empty ""
          testCase "wildcard pattern"
          <| fun _ ->
              let result =
                  filter
                      (Set.ofList
                          [ { element = ElementPattern.Variable(Variable "e")
                              attribute = ElementPattern.Variable(Variable "a")
                              value = ValuePattern.Variable(Variable "v") } ])
                      (Set.ofList
                          [ { element = Element "e"
                              attribute = Element "a"
                              value = Value.Element(Element "v") } ])

              Expect.equal
                  result
                  (Set.ofList
                      [ { element = Element "e"
                          attribute = Element "a"
                          value = Value.Element(Element "v") } ])
                  ""
          testCase "not matching pattern"
          <| fun _ ->
              let result =
                  filter
                      (Set.ofList
                          [ { element = ElementPattern.Variable(Variable "e")
                              attribute = ElementPattern.Variable(Variable "a")
                              value = ValuePattern.Variable(Variable "v") } ])
                      (Set.ofList
                          [ { element = Element "e"
                              attribute = Element "a"
                              value = Value.Element(Element "v") } ])

              Expect.equal
                  result
                  (Set.ofList
                      [ { element = Element "e"
                          attribute = Element "a"
                          value = Value.Element(Element "v") } ])
                  ""

          ]
