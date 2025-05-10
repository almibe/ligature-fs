// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Library

open Ligature.Model
open Wander.Model
open Wander.Fns.Expect
open Wander.Fns.Core
open Wander.Fns.Tuple
open Wander.Fns.Network
open Wander.Fns.Ligature
open Interpreter
open Wander.Fns.Remote
open Wander.Fns.Store
open InMemoryStore
open Wander.Fns.Bend

let docsFn: Fn =
    Fn(
        { doc = "Push the docs Network on the Stack."
          examples = []
          args = "()"
          result = "Assertions" },
        fun actions _ _ _ ->
            let docs: Assertions =
                Map.toSeq actions
                |> Seq.fold
                    (fun state (name, action) ->
                        match action with
                        | Fn(doc, _) ->
                            let state =
                                Set.add
                                    (Assertion.Triple(name, Term "doc-string", Value.Literal(Literal doc.doc)))
                                    state

                            let state = Set.add (Assertion.Triple(name, Term ":", Value.Term(Term "Fn"))) state

                            let state =
                                Set.add (Assertion.Triple(name, Term "args", Value.Literal(Literal doc.args))) state

                            let state =
                                Set.add
                                    (Assertion.Triple(name, Term "result", Value.Literal(Literal doc.result)))
                                    state

                            List.fold
                                (fun state example ->
                                    Set.add
                                        (Assertion.Triple(name, Term "doc-example", Value.Literal(Literal example)))
                                        state)
                                state
                                doc.examples)
                    Set.empty

            Ok(Any.Assertions docs)
    )

let stdFns (store: ILigatureStore) : Fns =
    Map.ofSeq
        [ Term "expect-equal", expectEqualFn
          Term "assertions", assertionsFn
          Term "instance", instanceFn
          Term "find-model", findModelFn
          Term "concept", conceptFn
          Term "not", notFn
          Term "and", andFn
          Term "or", orFn
          //   Term "pattern", patternFn
          Term "definitions", definitionsFn
          Term "equivalent", equivalentFn
          Term "all", allFn
          Term "exists", existsFn
          Term "implies", impliesFn
          Term "union", unionFn
          Term "remote", remoteFn
          Term "extract", extractFn
          Term "instances", instancesFn
          Term "docs", docsFn
          //   Term "prepend", prependFn
          Term "set", setFn
          Term "result-set", resultSetFn
          Term "id", idFn
          Term "if-empty", ifEmptyFn
          Term "is-empty", isEmptyFn
          Term "filter", filterFn
          Term "query", queryFn
          Term "count", countFn
          Term "bend.json.instances", bendJsonFn
          Term "unfold", unfoldFn
          Term "top", topFn
          Term "bottom", bottomFn
          Term "nnf", nnfFn
          Term "is-definitorial", isDefinitorialFn
          Term "is-consistent", isConsistentFn ]
    |> createStoreFns store

let mergeLibraries first second =
    Map.fold (fun state key value -> Map.add key value state) first second
