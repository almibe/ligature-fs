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
        (Term "stores")
        (Fn(
            { doc = "Returns a tuple of all the existing KBs."
              examples = [ "stores" ]
              args = ""
              result = "Tuple" },
            fun _ _ _ _ -> //TODO assert no args were passed
                store.KBs()
                |> Seq.map (fun value -> Expression.Term(Term value))
                |> List.ofSeq
                |> Expression.Tuple
                |> Ok
        ))
    |> Map.add
        (Term "add-store")
        (Fn(
            { doc = "Reads a Network name and creates a Network in the Store."
              examples = [ "add-store test" ]
              args = "Term"
              result = "" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term(Term name) ] ->
                    store.AddKB name
                    Ok(Expression.ABox Set.empty)
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "remove-store")
        (Fn(
            { doc = "Removes the given KB name from the store."
              examples = [ "remove-store test" ]
              args = "Term"
              result = "" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term(Term name) ] ->
                    store.RemoveKB(name)
                    Ok(Expression.ABox Set.empty)
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "assert-store")
        (Fn(
            { doc = "Given a KB name and a Network merge the network into the ABox for given KB."
              examples = [ "assert-store test {a b c}" ]
              args = "Literal Network"
              result = "" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term(Term networkName); Expression.ABox network ] ->
                    store.AssertKB networkName network
                    Ok(Expression.ABox Set.empty)
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "unassert-store")
        (Fn(
            { doc = "Given a KB name and a Network remove the network into the ABox for given KB."
              examples = [ "unassert-store test {a b c}" ]
              args = "Term Network"
              result = "" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term(Term networkName); Expression.ABox network ] ->
                    store.UnassertKB networkName network
                    Ok(Expression.ABox Set.empty)
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "read-assert-store")
        (Fn(
            { doc = "Read only the asserts in a KB."
              examples = [ "read-assert-store test" ]
              args = "Term"
              result = "Network" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term(Term networkName) ] ->
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
