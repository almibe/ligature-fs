// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Store

open Ligature.Model
open Wander.Model
open Ligature.Core
open Wander.Interpreter
open Ligature.Interpreter

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
                |> List.ofSeq
                |> Expression.Seq
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
                    Ok(Expression.Assertions Set.empty)
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
                    Ok(Expression.Assertions Set.empty)
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "assert")
        (Fn(
            { doc = "Given a KB name and a set of assertions, merge the assertions into the assertions for given KB."
              examples = [ "assert(test assertions([a b c]))" ]
              args = "Literal Assertions"
              result = "" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term networkName; Expression.Assertions network ] ->
                    store.AssertKB networkName network
                    Ok(Expression.Assertions Set.empty)
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "unassert")
        (Fn(
            { doc =
                "Given a KB name and a set of assertions, remove the assertions from the assertions for the given KB."
              examples = [ "unassert(test assertions([a b c]))" ]
              args = "Term Assertions"
              result = "" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term networkName; Expression.Assertions network ] ->
                    store.UnassertKB networkName network
                    Ok(Expression.Assertions Set.empty)
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "read-assertions")
        (Fn(
            { doc = "Read only the asserts in a KB."
              examples = [ "read-assertions(test)" ]
              args = "Term"
              result = "Assertions" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term networkName ] ->
                    match store.ReadAssertsKB networkName with
                    | Ok network -> Ok(Expression.Assertions network)
                    | Error err -> Error err
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "define")
        (Fn(
            { doc = "Given a KB name and a set of definitions, merge the definitions into the definitions for given KB."
              examples = [ "define(test definitions(equilavlent(A B)))" ]
              args = "Term Definitions"
              result = "" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term networkName; Expression.Definitions definitions ] ->
                    store.DefineKB networkName definitions
                    Ok(Expression.Assertions Set.empty)
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "undefine")
        (Fn(
            { doc =
                "Given a KB name and a set of definitions, remove the definitions from the definitions for the given KB."
              examples = [ "undefine(test definitions((equivalent A B)))" ]
              args = "Term Definitions"
              result = "" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term networkName; Expression.Definitions definitions ] ->
                    store.UndefineKB networkName definitions
                    Ok(Expression.Assertions Set.empty)
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "read-definitions")
        (Fn(
            { doc = "Read only the definitions in a KB."
              examples = [ "read-definitions(test)" ]
              args = "Term"
              result = "Definitions" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term networkName ] ->
                    match store.ReadDefinitionsKB networkName with
                    | Ok definitions -> Ok(Expression.Definitions definitions)
                    | _ -> failwith "TODO"
                | _ -> failwith "TODO"
        ))
    |> Map.add
        (Term "is-consistent")

        (Fn(
            { doc = "Check if a KB is consistent."
              examples = [ "(is-consistent (definitions (implies A B)) (assertions (instance a A)))" ]
              args = "Definitions Assertions"
              result = "Term" },
            fun _ _ _ arguments ->
                match arguments with
                | [ Expression.Term kbName ] ->
                    store.IsConsistent kbName
                    |> Result.map (fun value ->
                        match value with
                        | true -> Expression.Term(Term "true")
                        | false -> Expression.Term(Term "false"))
                | [ Expression.Definitions def; Expression.Assertions n ] ->
                    match isConsistent def n with
                    | Ok true -> Ok(Expression.Term(Term "true"))
                    | Ok false -> Ok(Expression.Term(Term "false"))
                    | Error err -> Error err
                | _ -> error "Invalid call to is-consistent." None
        ))
