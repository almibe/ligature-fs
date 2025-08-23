// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Log

open Ligature.Model
open Ligature.InMemoryStore
open FASTER.core
open System.IO
open Wander.Model
open Wander.Main
open Wander.Library

type LogStore(path: Option<string>) =

    let store =
        match path with
        | None ->
            let tempDir = Directory.CreateTempSubdirectory()
            let settings = new FasterLogSettings(tempDir.FullName, true)
            new FasterLog(settings)
        | Some path ->
            let settings = new FasterLogSettings(path)
            new FasterLog(settings)

    let inmem: ILigatureStore =
        match path with
        | Some _ ->
            let inmem = new InMemoryStore()
            let iter = store.Scan(store.BeginAddress, store.TailAddress)
            let mutable cont = true

            while cont do
                cont <-
                    iter.TryConsumeNext
                        { new ILogEntryConsumer with
                            member _.Consume(e, _, _) =
                                let command = System.Text.Encoding.UTF8.GetString e

                                match run (stdFns inmem) Map.empty command with
                                | Ok _ -> ()
                                | Error err -> failwith err.UserMessage }

            inmem
        | None -> new InMemoryStore()

    interface ILigatureStore with
        member this.KBs() : Term seq = inmem.KBs()

        member this.AddKB(name: Term) : unit =
            let encoded =
                printExpression (
                    Expression.Application
                        { name = Term "add-kb"
                          attributes = Map.empty
                          arguments = [ Expression.Term name ] }
                )

            store.Enqueue(System.Text.Encoding.UTF8.GetBytes(encoded)) |> ignore
            store.Commit()
            inmem.AddKB name

        member this.RemoveKB(name: Term) : unit =
            let encoded =
                printExpression (
                    Expression.Application
                        { name = Term "remove-kb"
                          attributes = Map.empty
                          arguments = [ Expression.Term name ] }
                )

            store.Enqueue(System.Text.Encoding.UTF8.GetBytes encoded) |> ignore
            store.Commit()
            inmem.RemoveKB name


        member this.AssertKB (name: Term) (newAssertions: Assertions) : unit =
            let encoded =
                printExpression (
                    Expression.Application
                        { name = Term "assert"
                          attributes = Map.empty
                          arguments = [ Expression.Term name; Expression.Assertions newAssertions ] }
                )

            store.Enqueue(System.Text.Encoding.UTF8.GetBytes encoded) |> ignore
            store.Commit()
            inmem.AssertKB name newAssertions

        member this.UnassertKB (name: Term) (network: Assertions) : unit =
            let encoded =
                printExpression (
                    Expression.Application
                        { name = Term "unassert"
                          attributes = Map.empty
                          arguments = [ Expression.Term name; Expression.Assertions network ] }
                )

            store.Enqueue(System.Text.Encoding.UTF8.GetBytes encoded) |> ignore
            store.Commit()
            inmem.UnassertKB name network

        member this.ReadAssertsKB(name: Term) : Result<Assertions, LigatureError> = inmem.ReadAssertsKB name

        member this.ReadDefinitionsKB(name: Term) : Result<Definitions, LigatureError> = inmem.ReadDefinitionsKB name

        member this.DefineKB (name: Term) (newDefinitions: Definitions) : unit =
            let encoded =
                printExpression (
                    Expression.Application
                        { name = Term "define"
                          attributes = Map.empty
                          arguments = [ Expression.Term name; Expression.Definitions newDefinitions ] }
                )

            store.Enqueue(System.Text.Encoding.UTF8.GetBytes encoded) |> ignore
            store.Commit()
            inmem.DefineKB name newDefinitions

        member this.UndefineKB (name: Term) (toRemove: Definitions) : unit =
            let encoded =
                printExpression (
                    Expression.Application
                        { name = Term "undefine"
                          attributes = Map.empty
                          arguments = [ Expression.Term name; Expression.Definitions toRemove ] }
                )

            store.Enqueue(System.Text.Encoding.UTF8.GetBytes encoded) |> ignore
            store.Commit()
            inmem.UndefineKB name toRemove

        member this.ReadKB(arg: Term) : Result<KnowledgeBase, LigatureError> = inmem.ReadKB arg

        member this.IsConsistent(name: Term) : Result<bool, LigatureError> = inmem.IsConsistent name

        member this.IsEquivalent (arg1: Term) (arg2: ConceptExpr) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            inmem.IsEquivalent arg1 arg2 arg3

        member this.IsInstance (arg1: Term) (arg2: Element) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            inmem.IsInstance arg1 arg2 arg3

        member this.IsSatisfiable (arg1: Term) (arg2: ConceptExpr) : Result<bool, LigatureError> =
            inmem.IsSatisfiable arg1 arg2

        member this.IsSubsumedBy (arg1: Term) (arg2: ConceptExpr) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            inmem.IsSubsumedBy arg1 arg2 arg3

        member this.Instances (arg1: Term) (arg2: ConceptExpr) : Result<Element seq, LigatureError> =
            inmem.Instances arg1 arg2

        member this.Query (arg: Term) (arg_1: Pattern) : Result<ResultSet, LigatureError> =
            raise (System.NotImplementedException())

        member this.TableauModels(arg1: Term) : Result<Set<Assertions>, LigatureError> = inmem.TableauModels arg1
