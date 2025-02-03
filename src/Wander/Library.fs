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
                |> Seq.map (fun (name, action) ->
                    match action with
                    | Action.Full(doc, _) ->
                        (ElementPattern.Element name,
                         ElementPattern.Element(Element "doc-string"),
                         Value.Literal doc.doc)
                    | Action.Stack(doc, _) ->
                        (ElementPattern.Element name,
                         ElementPattern.Element(Element "doc-string"),
                         Value.Literal doc.doc))
                |> Set.ofSeq

            Ok(networks, Any.Network docs :: stack)
    )

let stdActions: Actions =
    Map.ofSeq
        [ (Element "assert-equal", assertEqualAction)
          (Element "union", unionAction)
          (Element "infer", inferAction)
          (Element "to-json", toJSONAction)
          (Element "docs", docsAction)
          (Element "prepend", prependAction)
          (Element "clear", clearAction)
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
