// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Library

open Ligature.Model
open Wander.Model
open Wander.Fns.Expect
open Wander.Fns.Core
open Wander.Fns.Network
open Wander.Fns.Ligature
open Wander.Fns.Remote
open Wander.Fns.Store
open Wander.Fns.Ulid

let docsFn: Fn =
    Fn(
        { doc = "Push the docs Network on the Stack."
          examples = []
          args = "()"
          result = "Assertions" },
        fun actions _ _ _ -> failwith "TODO"
    // let docs: ABox =
    //     Map.toSeq actions
    //     |> Seq.fold
    //         (fun state (name, action) ->
    //             match action with
    //             | Fn(doc, _) ->
    //                 let state =
    //                     Set.add
    //                         (Assertion.Triple(
    //                             name,
    //                             Term "doc-string",
    //                             Value.Literal
    //                                 { id = doc.doc
    //                                   datatype = None
    //                                   langTag = None }
    //                         ))
    //                         state

    //                 let state = Set.add (Assertion.Triple(name, Term ":", Value.Term(Term "Fn"))) state

    //                 let state =
    //                     Set.add
    //                         (Assertion.Triple(
    //                             name,
    //                             Term "args",
    //                             Value.Literal
    //                                 { id = doc.args
    //                                   datatype = None
    //                                   langTag = None }
    //                         ))
    //                         state

    //                 let state =
    //                     Set.add
    //                         (Assertion.Triple(
    //                             name,
    //                             Term "result",
    //                             Value.Literal
    //                                 { id = doc.result
    //                                   datatype = None
    //                                   langTag = None }
    //                         ))
    //                         state

    //                 List.fold
    //                     (fun state example ->
    //                         Set.add
    //                             (Assertion.Triple(
    //                                 name,
    //                                 Term "doc-example",
    //                                 Value.Literal
    //                                     { id = example
    //                                       datatype = None
    //                                       langTag = None }
    //                             ))
    //                             state)
    //                     state
    //                     doc.examples)
    //         Set.empty

    // Ok(Expression.ABox docs)
    )

let stdFns (store: ILigatureStore) : Fns =
    Map.ofSeq
        [ Term "test-group", testGroupFn
          Term "expect-equal", expectEqualFn
          Term "ulid", ulidFn
          Term "a-box", aBoxFn
          Term "individual", individualFn
          Term "instance", instanceFn
          Term "same", sameFn
          Term "different", differentFn
          Term "find-model", findModelFn
          Term "tableau-models", tableauModelsFn
          Term "concept", conceptFn
          Term "not", notFn
          Term "and", andFn
          Term "or", orFn
          Term "t-box", tBoxFn
          Term "equivalent", equivalentFn
          Term "all", allFn
          Term "exists", existsFn
          Term "func", funcFn
          //   Term "exactly", exactlyFn
          //   Term "at-least", atLeastFn
          //   Term "at-most", atMostFn
          Term "implies", impliesFn
          Term "union", unionFn
          Term "remote", remoteFn
          Term "docs", docsFn
          Term "id", idFn
          Term "is-empty", isEmptyFn
          Term "query", queryFn
          Term "count", countFn
          Term "unfold", unfoldFn
          Term "top", topFn
          Term "bottom", bottomFn
          Term "nnf", nnfFn
          Term "is-instance", isInstanceFn
          Term "is-definitorial", isDefinitorialFn
          Term "is-consistent", isConsistentFn ]
    |> createStoreFns store

let mergeLibraries first second =
    Map.fold (fun state key value -> Map.add key value state) first second
