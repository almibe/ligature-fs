// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Library

open Ligature.Model
open Wander.Model
open Wander.Fns.Assert
open Wander.Fns.Core
open Wander.Fns.Quote
open Wander.Fns.Network
open Wander.Fns.TinyDL
open Interpreter
open Wander.Fns.Remote

let docsFn: Fn =
    Fn(
        { doc = "Push the docs Network on the Stack."
          examples = []
          pre = ""
          post = "Network" },
        fun actions variables arguments -> failwith "TODO"
    // let docs: Pattern =
    //     Map.toSeq actions
    //     |> Seq.fold
    //         (fun state (name, action) ->
    //             match action with
    //             | Fn.Full(doc, _) ->
    //                 let state =
    //                     Set.add
    //                         (TermPattern.Term name,
    //                          TermPattern.Term(Term "doc-string"),
    //                          TermPattern.Term(Term doc.doc))
    //                         state

    //                 let state =
    //                     Set.add
    //                         (TermPattern.Term name,
    //                          TermPattern.Term(Term ":"),
    //                          TermPattern.Term(Term "Fn"))
    //                         state

    //                 let state =
    //                     Set.add
    //                         (TermPattern.Term name,
    //                          TermPattern.Term(Term "doc-pre"),
    //                          TermPattern.Term(Term doc.pre))
    //                         state

    //                 let state =
    //                     Set.add
    //                         (TermPattern.Term name,
    //                          TermPattern.Term(Term "doc-post"),
    //                          TermPattern.Term(Term doc.post))
    //                         state

    //                 List.fold
    //                     (fun state example ->
    //                         Set.add
    //                             (TermPattern.Term name,
    //                              TermPattern.Term(Term "doc-example"),
    //                              TermPattern.Term(Term example))
    //                             state)
    //                     state
    //                     doc.examples
    //             | Fn.Stack(doc, _) ->
    //                 let state =
    //                     Set.add
    //                         (TermPattern.Term name,
    //                          TermPattern.Term(Term ":"),
    //                          TermPattern.Term(Term "Fn"))
    //                         state

    //                 let state =
    //                     Set.add
    //                         (TermPattern.Term name,
    //                          TermPattern.Term(Term "doc-string"),
    //                          TermPattern.Term(Term doc.doc))
    //                         state

    //                 let state =
    //                     Set.add
    //                         (TermPattern.Term name,
    //                          TermPattern.Term(Term "doc-pre"),
    //                          TermPattern.Term(Term doc.pre))
    //                         state

    //                 let state =
    //                     Set.add
    //                         (TermPattern.Term name,
    //                          TermPattern.Term(Term "doc-post"),
    //                          TermPattern.Term(Term doc.post))
    //                         state

    //                 List.fold
    //                     (fun state example ->
    //                         Set.add
    //                             (TermPattern.Term name,
    //                              TermPattern.Term(Term "doc-example"),
    //                              TermPattern.Term(Term example))
    //                             state)
    //                     state
    //                     doc.examples)
    //         Set.empty

    // Ok(Any.Network docs :: stack)
    )

let stdFns: Fns =
    Map.ofSeq
        [ (Term "assert-equal", assertEqualFn)
          (Term "union", unionFn)
          (Term "infer", inferFn)
          (Term "remote", remoteFn)
          (Term "extract", extractFn)
          (Term "extract-json", extractJsonFn)
          (Term "instances", instancesFn)
          (Term "instances-json", instancesJsonFn)
          (Term "docs", docsFn)
          (Term "prepend", prependFn)
          (Term "set", setFn)
          (Term "pop", popFn)
          (Term "if-empty", ifEmptyFn)
          (Term "is-empty", isEmptyFn)
          (Term "filter", filterFn)
          (Term "query", queryFn)
          (Term "count", countFn) ]
// (Term "is-consistent",
//  createFn
//      "Check if the Network on the top of the Stack is consistent."
//      [ Any.Network(
//            Set.ofList
//                [ (TermPattern.Slot(Slot "?el"), TermPattern.Term(Term ":"), TermPattern.Slot(Slot "?concept"))
//                  (TermPattern.Slot(Slot "?el"), TermPattern.Term(Term ":¬"), TermPattern.Slot(Slot "?concept")) ]
//        )
//        Any.Network(
//            Set.ofList
//                [ TermPattern.Slot(Slot "?el"), TermPattern.Term(Term ":¬"), TermPattern.Slot(Slot "?concept") ]
//        )
//        Any.Term(Term "query")
//        Any.Term(Term "is-empty") ]
//      []
//      ""
//      "") ]
