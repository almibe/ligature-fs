// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Library

open Ligature.Model
open Wander.Model
open Wander.Actions.Assert
open Wander.Actions.Core
open Wander.Actions.Quote
open Wander.Actions.JSON
open Wander.Actions.Network
open Wander.Actions.TinyDL
open Interpreter
open Wander.Actions.Store

let docsAction: Action =
    Action.Full(
        { doc = "Push the docs Network on the Stack."
          examples = [] },
        fun actions networks stack ->
            let docs: Network =
                Map.toSeq actions
                |> Seq.fold (fun state (name, action) ->
                    match action with
                    | Action.Full(doc, _) ->
                        let state =
                            Set.add (ElementPattern.Element name,
                            ElementPattern.Element(Element "doc-string"),
                            Value.Literal doc.doc) state
                        let state =
                            Set.add (ElementPattern.Element name,
                            ElementPattern.Element(Element ":"),
                            Value.Element (Element "Action") ) state
                        List.fold (fun state example -> 
                            Set.add (ElementPattern.Element name,
                            ElementPattern.Element(Element "doc-example"),
                            Value.Literal example) state) state doc.examples
                    | Action.Stack(doc, _) ->
                        let state =
                            Set.add (ElementPattern.Element name,
                            ElementPattern.Element(Element ":"),
                            Value.Element (Element "Action") ) state
                        let state = 
                            Set.add (ElementPattern.Element name,
                            ElementPattern.Element(Element "doc-string"),
                            Value.Literal doc.doc) state
                        List.fold (fun state example ->
                            Set.add (ElementPattern.Element name,
                            ElementPattern.Element(Element "doc-example"),
                            Value.Literal example) state) state doc.examples) Set.empty

            Ok(networks, Any.Network docs :: stack)
    )

let stdActions: Actions =
    Map.ofSeq
        [ (Element "assert-equal", assertEqualAction)
          (Element "union", unionAction)
          (Element "infer", inferAction)
          (Element "extract", extractAction)
          (Element "extract-json", extractJsonAction)
          (Element "instances", instancesAction)
          (Element "instances-json", instancesJsonAction)
          (Element "to-json", toJSONAction)
          (Element "docs", docsAction)
          (Element "prepend", prependAction)
          (Element "clear", clearAction)
          (Element "set", setAction)
          (Element "pop", popAction)
          (Element "if-empty", ifEmptyAction)
          (Element "is-empty", isEmptyAction)
          (Element "filter", filterAction)
          (Element "query", queryAction)
          (Element "count", countAction)
          (Element "merge", mergeAction)
          (Element "remove", removeAction)
          (Element "read", readAction)
          (Element "is-consistent",
           createAction
               "Check if the Network on the top of the Stack is consistent."
               [ Any.Network(
                     Set.ofList
                         [ (ElementPattern.Variable(Variable "?el"),
                            ElementPattern.Element(Element ":"),
                            Value.Variable(Variable "?concept"))
                           (ElementPattern.Variable(Variable "?el"),
                            ElementPattern.Element(Element ":¬"),
                            Value.Variable(Variable "?concept")) ]
                 )
                 Any.Network(
                     Set.ofList
                         [ ElementPattern.Variable(Variable "?el"),
                           ElementPattern.Element(Element ":¬"),
                           Value.Variable(Variable "?concept") ]
                 )
                 Any.Element(Element "query")
                 Any.Element(Element "is-empty") ]) ]
