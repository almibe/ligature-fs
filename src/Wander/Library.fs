// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Library

open Ligature.Model
open Wander.Model
open Wander.Fns.Expect
open Wander.Fns.Core
open Wander.Fns.Assertions
open Wander.Fns.Html
open Wander.Fns.Ligature
open Wander.Fns.Store
open Wander.Fns.Ulid

let docsFn: Fn =
    Fn.Fn(
        { doc = "Push the docs Network on the Stack."
          examples = []
          args = "()"
          result = "Assertions" },
        fun actions _ _ ->
            let docs: Assertions =
                Map.toSeq actions
                |> Seq.fold
                    (fun state (Term name, action) ->
                        let name =
                            { value = name
                              space = None
                              langTag = None }

                        match action with
                        | Fn.Fn(doc, _) ->
                            let state =
                                Set.add
                                    (Assertion.Triple(
                                        name,
                                        Term "doc-string",
                                        { value = doc.doc
                                          space = None
                                          langTag = None }
                                    ))
                                    state

                            let state =
                                Set.add
                                    (Assertion.Triple(
                                        name,
                                        Term ":",
                                        { value = "Fn"
                                          space = None
                                          langTag = None }
                                    ))
                                    state

                            let state =
                                Set.add
                                    (Assertion.Triple(
                                        name,
                                        Term "args",
                                        { value = doc.args
                                          space = None
                                          langTag = None }
                                    ))
                                    state

                            let state =
                                Set.add
                                    (Assertion.Triple(
                                        name,
                                        Term "result",
                                        { value = doc.result
                                          space = None
                                          langTag = None }
                                    ))
                                    state

                            List.fold
                                (fun state example ->
                                    Set.add
                                        (Assertion.Triple(
                                            name,
                                            Term "doc-example",
                                            { value = example
                                              space = None
                                              langTag = None }
                                        ))
                                        state)
                                state
                                doc.examples)
                    Set.empty

            Ok(Expression.Assertions docs)
    )

let stdFns (store: ILigatureStore) : Fns =
    Map.ofSeq
        [ Term "test-group", testGroupFn
          Term "expect-equal", expectEqualFn
          Term "ulid", ulidFn
          Term "fn", fnFn
          Term "do", doFn
          Term "pipe", pipeFn
          Term "map", mapFn
          Term "seq", seqFn
          Term "assertions", assertionsFn
          Term "element", elementFn
          Term "instance", instanceFn
          Term "triple", tripleFn
          Term "same", sameFn
          Term "different", differentFn
          //   Term "find-model", findModelFn
          Term "tableau-models", tableauModelsFn
          //   Term "concept", conceptFn
          Term "not", notFn
          Term "and", andFn
          Term "or", orFn
          Term "definitions", definitionsFn
          Term "equivalent", equivalentFn
          Term "all", allFn
          Term "exists", existsFn
          Term "func", funcFn
          //   Term "exactly", exactlyFn
          //   Term "at-least", atLeastFn
          //   Term "at-most", atMostFn
          Term "implies", impliesFn
          //   Term "union", unionFn
          Term "docs", docsFn
          Term "id", idFn
          //   Term "is-empty", isEmptyFn
          //   Term "query", queryFn
          Term "count", countFn
          Term "unfold", unfoldFn
          Term "top", topFn
          Term "bottom", bottomFn
          Term "nnf", nnfFn
          Term "is-instance", isInstanceFn
          Term "is-definitorial", isDefinitorialFn
          Term "generate-html", generateHtmlFn
          Term "force-directed-layout", forceDirectedLayoutFn
          Term "assertions-table", assertionsTableFn ]
    |> createStoreFns store

let mergeLibraries first second =
    Map.fold (fun state key value -> Map.add key value state) first second
