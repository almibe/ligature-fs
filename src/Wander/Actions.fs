// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Lib

open Ligature.Model
open Wander.Model
open Wander.Actions.Assert
open Wander.Actions.Core
open Wander.Actions.Network
open Wander.Actions.TinyDL
open Interpreter

let createAction (quote: Quote): Action =
  { Eval = 
      fun actions networks stack ->
        evalScript actions networks stack quote }

let stdActions: Actions =
    Map.ofSeq
        [ (Element "assert-equal", assertEqualAction)
          (Element "union", unionAction)
          (Element "infer", inferAction)
          (Element "clear", clearAction)
          (Element "pop", popAction)
          (Element "if-empty", ifEmptyAction)
          (Element "is-empty", isEmptyAction)
          (Element "filter", filterAction)
          (Element "query", queryAction)
          (Element "count", countAction) 
          (Element "is-consistent", createAction [
            Any.Network(
                Set.ofList
                    [ ElementPattern.Variable(Variable "?el"),
                      ElementPattern.Element(Element ":"),
                      Value.Variable(Variable "?concept") ])
            Any.Network(
                Set.ofList
                    [ ElementPattern.Variable(Variable "?el"),
                      ElementPattern.Element(Element ":¬"),
                      Value.Variable(Variable "?concept") ])
            Any.Element (Element "query")
            Any.Literal "true"
            Any.Literal "false"
            Any.Element(Element "if-empty")
          ]) ]
