// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Library

open Ligature.Model
open Wander.Model
open Wander.Actions.Assert
open Wander.Actions.Core
open Wander.Actions.Quote
open Wander.Actions.Network
open Wander.Actions.TinyDL
open Interpreter
open Wander.Actions.Remote

let docsAction: Action =
    Action.Full(
        { doc = "Push the docs Network on the Stack."
          examples = []
          pre = ""
          post = "Network" },
        fun actions stack ->
            let docs: Pattern =
                Map.toSeq actions
                |> Seq.fold
                    (fun state (name, action) ->
                        match action with
                        | Action.Full(doc, _) ->
                            let state =
                                Set.add
                                    (TermPattern.Term name,
                                     TermPattern.Term(Term "doc-string"),
                                     TermPattern.Term(Term doc.doc))
                                    state

                            let state =
                                Set.add
                                    (TermPattern.Term name,
                                     TermPattern.Term(Term ":"),
                                     TermPattern.Term(Term "Action"))
                                    state

                            let state =
                                Set.add
                                    (TermPattern.Term name,
                                     TermPattern.Term(Term "doc-pre"),
                                     TermPattern.Term(Term doc.pre))
                                    state

                            let state =
                                Set.add
                                    (TermPattern.Term name,
                                     TermPattern.Term(Term "doc-post"),
                                     TermPattern.Term(Term doc.post))
                                    state

                            List.fold
                                (fun state example ->
                                    Set.add
                                        (TermPattern.Term name,
                                         TermPattern.Term(Term "doc-example"),
                                         TermPattern.Term(Term example))
                                        state)
                                state
                                doc.examples
                        | Action.Stack(doc, _) ->
                            let state =
                                Set.add
                                    (TermPattern.Term name,
                                     TermPattern.Term(Term ":"),
                                     TermPattern.Term(Term "Action"))
                                    state

                            let state =
                                Set.add
                                    (TermPattern.Term name,
                                     TermPattern.Term(Term "doc-string"),
                                     TermPattern.Term(Term doc.doc))
                                    state

                            let state =
                                Set.add
                                    (TermPattern.Term name,
                                     TermPattern.Term(Term "doc-pre"),
                                     TermPattern.Term(Term doc.pre))
                                    state

                            let state =
                                Set.add
                                    (TermPattern.Term name,
                                     TermPattern.Term(Term "doc-post"),
                                     TermPattern.Term(Term doc.post))
                                    state

                            List.fold
                                (fun state example ->
                                    Set.add
                                        (TermPattern.Term name,
                                         TermPattern.Term(Term "doc-example"),
                                         TermPattern.Term(Term example))
                                        state)
                                state
                                doc.examples)
                    Set.empty

            Ok(Any.Network docs :: stack)
    )

let stdActions: Actions =
    Map.ofSeq
        [ (Term "assert-equal", assertEqualAction)
          (Term "union", unionAction)
          (Term "infer", inferAction)
          (Term "remote", remoteAction)
          (Term "extract", extractAction)
          (Term "extract-json", extractJsonAction)
          (Term "instances", instancesAction)
          (Term "instances-json", instancesJsonAction)
          (Term "docs", docsAction)
          (Term "prepend", prependAction)
          (Term "clear", clearAction)
          (Term "set", setAction)
          (Term "pop", popAction)
          (Term "if-empty", ifEmptyAction)
          (Term "is-empty", isEmptyAction)
          (Term "filter", filterAction)
          (Term "query", queryAction)
          (Term "count", countAction)
          (Term "is-consistent",
           createAction
               "Check if the Network on the top of the Stack is consistent."
               [ Any.Network(
                     Set.ofList
                         [ (TermPattern.Variable(Slot "?el"),
                            TermPattern.Term(Term ":"),
                            TermPattern.Variable(Slot "?concept"))
                           (TermPattern.Variable(Slot "?el"),
                            TermPattern.Term(Term ":¬"),
                            TermPattern.Variable(Slot "?concept")) ]
                 )
                 Any.Network(
                     Set.ofList
                         [ TermPattern.Variable(Slot "?el"),
                           TermPattern.Term(Term ":¬"),
                           TermPattern.Variable(Slot "?concept") ]
                 )
                 Any.Element(Term "query")
                 Any.Element(Term "is-empty") ]
               []
               ""
               "") ]
