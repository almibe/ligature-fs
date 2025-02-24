// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Kb

open Ligature.Model
open Wander.Model
open Ligature.Core
open Wander.Interpreter

let createStoreFns (store: ILigatureStore) (baseFns: Fns) : Fns =
    baseFns
    |> Map.add
        (Term "kbs")
        (Fn(
            { doc = "Returns a quote of all the existing KBs."
              examples = [ "kbs" ]
              pre = ""
              post = "Quote" },
            fun fns variables arguments -> //TODO assert no args were passed
                store.KnowledgeBases()
                |> Seq.map (fun value -> Any.Term(Term value))
                |> List.ofSeq
                |> Any.Quote
                |> Ok
        ))
        |> Map.add
            (Term "add-kb")
            (Fn(
                { doc = "Reads a Network name and creates a Network in the Store."
                  examples = [ "add-kb test" ]
                  pre = "Term"
                  post = "" },
                fun fns variables arguments ->
                    match arguments with
                    | [Any.Term (Term name)] ->
                        store.AddKnowledgeBase name
                        Ok(Any.Network Set.empty)
                    | _ -> failwith "TODO"
            ))
        |> Map.add
            (Term "remove-kb")
            (Fn(
                { doc = "Removes the given KB name from the store."
                  examples = [ "remove-kb test" ]
                  pre = "Term"
                  post = "" },
                fun fns variables arguments ->
                    match arguments with
                    | [Any.Term (Term name)] ->
                        store.RemoveKnowledgeBase(name)
                        Ok(Any.Network Set.empty)
                    | _ -> failwith "TODO"
            ))
        |> Map.add 
            (Term "assert-kb")
            (Fn(
                { doc = "Given a KB name and a Network merge the network into the ABox for given KB."
                  examples = [ "assert-kb test {a b c}" ]
                  pre = "Literal Network"
                  post = "" },
                fun actions variables arguments ->                   
                  match arguments with
                  | [ Any.Term (Term networkName); Any.Network network ] ->
                      store.AssertKnowledgeBase networkName network
                      Ok (Any.Network Set.empty)
                  | _ -> failwith "TODO")
            )
        |> Map.add 
            (Term "define-kb")
            (Fn(
                { doc = "Given a KB name and a Network merge the network into the TBox for given KB."
                  examples = [ "define-kb test {A subconcept-of B}" ]
                  pre = "Literal Network"
                  post = "" },
                fun actions variables arguments ->                   
                  match arguments with
                  | [ Any.Term (Term networkName); Any.Network network ] ->
                      store.DefineKnowledgeBase networkName network
                      Ok (Any.Network Set.empty)
                  | _ -> failwith "TODO")
            )
        |> Map.add 
            (Term "unassert-kb")
            (Fn(
                { doc = "Given a KB name and a Network remove the network into the ABox for given KB."
                  examples = [ "unassert-kb test {a b c}" ]
                  pre = "Term Network"
                  post = "" },
                fun actions variables arguments ->                   
                  match arguments with
                  | [ Any.Term (Term networkName); Any.Network network ] ->
                      store.UnassertKnowledgeBase networkName network
                      Ok (Any.Network Set.empty)
                  | _ -> failwith "TODO")
            )
        |> Map.add 
            (Term "undefine-kb")
            (Fn(
                { doc = "Given a KB name and a Network remove the network into the TBox for given KB."
                  examples = [ "undefine-kb test {A subconcept-of B}" ]
                  pre = "Term Network"
                  post = "" },
                fun actions variables arguments ->                   
                  match arguments with
                  | [ Any.Term (Term networkName); Any.Network network ] ->
                      store.UndefineKnowledgeBase networkName network
                      Ok (Any.Network Set.empty)
                  | _ -> failwith "TODO")
            )
        |> Map.add
            (Term "read-kb")
            (Fn(
                { doc = "Read a KB."
                  examples = [ "read test" ]
                  pre = "Term"
                  post = "Network" },
                fun fns variables arguments ->
                    match arguments with
                    | [Any.Term (Term networkName)] -> 
                      match store.Read networkName with
                      | Ok network -> Ok(Any.Network(network))
                      | _ -> failwith "TODO"
                    | _ -> failwith "TODO"
        ))
        |> Map.add
            (Term "read-assert-kb")
            (Fn(
                { doc = "Read only the asserts in a KB."
                  examples = [ "read-assert-kb test" ]
                  pre = "Term"
                  post = "Network" },
                fun fns variables arguments ->
                    match arguments with
                    | [Any.Term (Term networkName)] -> 
                      match store.ReadAsserts networkName with
                      | Ok network -> Ok(Any.Network network)
                      | _ -> failwith "TODO"
                    | _ -> failwith "TODO"
        ))
        |> Map.add
            (Term "read-define-kb")
            (Fn(
                { doc = "Read only the definitions in a KB."
                  examples = [ "read-define-kb test" ]
                  pre = "Term"
                  post = "Network" },
                fun fns variables arguments ->
                    match arguments with
                    | [Any.Term (Term networkName)] -> 
                      match store.ReadDefinitions networkName with
                      | Ok network -> Ok(Any.Network network)
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
