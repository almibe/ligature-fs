// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Store

open Ligature.Model
open Wander.Model
open Ligature.Core
open Wander.Interpreter

let createStoreFns (store: ILigatureStore) (baseFns: Fns) : Fns =
    baseFns
    |> Map.add
        (Term "kbs")
        (Fn(
            { doc = "Returns a set of all the existing KBs."
              examples = [ "kbs()" ]
              args = ""
              result = "Set" },
            fun _ _ _ _ -> //TODO assert no args were passed
                store.KBs()
                |> Seq.map (fun value -> Expression.Term value)
                |> Set.ofSeq
                |> Expression.Set
                |> Ok
        ))
    |> Map.add
        (Term "add-kb")
        (Fn(
            { doc = "Reads a name and creates a KB in the Store."
              examples = [ "add-kb(test)" ]
              args = "Term"
              result = "" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term name ] ->
                    store.AddKB name
                    Ok(Expression.ABox Set.empty)
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "remove-kb")
        (Fn(
            { doc = "Removes the given KB name from the store."
              examples = [ "remove-kb(test)" ]
              args = "Term"
              result = "" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term name ] ->
                    store.RemoveKB name
                    Ok(Expression.ABox Set.empty)
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "assert-kb")
        (Fn(
            { doc = "Given a KB name and a set of assertions, merge the assertions into the assertions for given KB."
              examples = [ "assert-kb(test assertions([a b c]))" ]
              args = "Literal Assertions"
              result = "" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term networkName; Expression.ABox network ] ->
                    store.AssertKB networkName network
                    Ok(Expression.ABox Set.empty)
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "unassert-kb")
        (Fn(
            { doc =
                "Given a KB name and a set of assertions, remove the assertions from the assertions for the given KB."
              examples = [ "unassert-kb(test assertions([a b c]))" ]
              args = "Term Assertions"
              result = "" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term networkName; Expression.ABox network ] ->
                    store.UnassertKB networkName network
                    Ok(Expression.ABox Set.empty)
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "read-assertions-kb")
        (Fn(
            { doc = "Read only the asserts in a KB."
              examples = [ "read-assertions-kb(test)" ]
              args = "Term"
              result = "Assertions" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term networkName ] ->
                    match store.ReadAssertsKB networkName with
                    | Ok network -> Ok(Expression.ABox network)
                    | _ -> failwith "TODO"
                | _ -> failwith "TODO"
        ))

// |> Map.add
//     (Term "delete")
//     (Fn.Stack(
//         { doc =
//             "Reads a Network off the Stack and removes all of the Triples in that Network from the target Network."
//           examples = []
//           pre = "Network"
//           post = "" },
//         fun stack -> failwith "TODO"
//     ))
