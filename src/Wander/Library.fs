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
open Wander.Fns.Kb
open InMemoryStore
open Wander.Fns.Bend

let docsFn: Fn =
    Fn(
        { doc = "Push the docs Network on the Stack."
          examples = []
          args = ""
          result = "Network" },
        fun actions variables arguments ->
            let docs: Network =
                Map.toSeq actions
                |> Seq.fold
                    (fun state (name, action) ->
                        match action with
                        | Fn(doc, _) ->
                            let state = Set.add (name, Term "doc-string", Value.Literal(Literal doc.doc)) state

                            let state = Set.add (name, Term ":", Value.Term(Term "Fn")) state

                            let state = Set.add (name, Term "doc-pre", Value.Literal(Literal doc.args)) state

                            let state = Set.add (name, Term "doc-post", Value.Literal(Literal doc.result)) state

                            List.fold
                                (fun state example ->
                                    Set.add (name, Term "doc-example", Value.Literal(Literal example)) state)
                                state
                                doc.examples)
                    // | Fn.Stack(doc, _) ->
                    //     let state =
                    //         Set.add
                    //             (TermPattern.Term name,
                    //             TermPattern.Term(Term ":"),
                    //             TermPattern.Term(Term "Fn"))
                    //             state

                    //     let state =
                    //         Set.add
                    //             (TermPattern.Term name,
                    //             TermPattern.Term(Term "doc-string"),
                    //             TermPattern.Term(Term doc.doc))
                    //             state

                    //     let state =
                    //         Set.add
                    //             (TermPattern.Term name,
                    //             TermPattern.Term(Term "doc-pre"),
                    //             TermPattern.Term(Term doc.pre))
                    //             state

                    //     let state =
                    //         Set.add
                    //             (TermPattern.Term name,
                    //             TermPattern.Term(Term "doc-post"),
                    //             TermPattern.Term(Term doc.post))
                    //             state

                    // List.fold
                    //     (fun state example ->
                    //         Set.add
                    //             (TermPattern.Term name,
                    //             TermPattern.Term(Term "doc-example"),
                    //             TermPattern.Term(Term example))
                    //             state)
                    //     state
                    //     doc.examples)
                    Set.empty

            Ok(Any.Network docs)
    )

let stdFns (store: ILigatureStore) : Fns =
    Map.ofSeq
        [ Term "assert-equal", assertEqualFn
          Term "network", networkFn
          Term "pattern", patternFn
          Term "union", unionFn
          Term "infer", inferFn
          Term "remote", remoteFn
          Term "extract", extractFn
          Term "instances", instancesFn
          Term "docs", docsFn
          Term "prepend", prependFn
          Term "set", setFn
          Term "id", idFn
          Term "if-empty", ifEmptyFn
          Term "is-empty", isEmptyFn
          Term "filter", filterFn
          Term "query", queryFn
          Term "count", countFn
          Term "bend.json.instances", bendJsonFn
          Term "is-consistent", isConsistentFn ]
    |> createStoreFns store

let mergeLibraries first second =
    Map.fold (fun state key value -> Map.add key value state) first second
