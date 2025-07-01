// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Store

open Ligature.Model
open FASTER.core
open Wander.Model
open Interpreter

type LigatureStore(db: FasterKV<string, string>) =
    interface ILigatureStore with
        member this.KBs() : Term seq =
            let f = new SimpleFunctions<string, string>()
            use session = db.NewSession f
            let iter = session.Iterate()
            let mutable cont = true
            let mutable results = Set.empty

            while cont do
                let res, _ = iter.GetNext()

                if res then
                    results <- Set.add (Term(iter.GetKey().Remove(0, 2))) results
                else
                    cont <- false

            List.ofSeq results

        member this.AddKB(Term name) : unit =
            let f = new SimpleFunctions<string, string>()
            use session = db.NewSession f
            session.Upsert($"A:{name}", "assertions()") |> ignore
            session.Upsert($"D:{name}", "definitions()") |> ignore
            session.CompletePending() |> ignore

        member this.RemoveKB(Term name) : unit =
            let f = new SimpleFunctions<string, string>()
            use session = db.NewSession f
            session.Delete $"A:{name}" |> ignore
            session.Delete $"D:{name}" |> ignore
            session.CompletePending() |> ignore

        member this.AssertKB (Term name) (assertions: Assertions) =
            let f = new SimpleFunctions<string, string>()
            use session = db.NewSession f

            //TODO read current assertions
            let status, value = session.Read $"A:{name}"

            if status.Found then
                match Wander.Main.runWithDefaults value with
                | Ok(Expression.Assertions prevAssertions) ->
                    let newAssertions = Set.union assertions prevAssertions
                    let value = printAny (Expression.Assertions newAssertions)
                    session.Upsert($"A:{name}", value) |> ignore
                    session.CompletePending() |> ignore
                | _ -> failwith "TODO"
            else
                failwith "TODO"

        member this.DefineKB (Term name) (definitions: Definitions) : unit =
            let f = new SimpleFunctions<string, string>()
            use session = db.NewSession f

            let status, value = session.Read $"D:{name}"

            if status.Found then
                match Wander.Main.runWithDefaults value with
                | Ok(Expression.Definitions prevDefinitions) ->
                    let newDefinitions = Set.union definitions prevDefinitions
                    let value = printAny (Expression.Definitions newDefinitions)
                    session.Upsert($"D:{name}", value) |> ignore
                    session.CompletePending() |> ignore
                | _ -> failwith "TODO"
            else
                failwith "TODO"

        member this.ReadAssertsKB(Term name) : Result<Assertions, LigatureError> =
            let f = new SimpleFunctions<string, string>()
            use session = db.NewSession f
            let status, assertions = session.Read $"A:{name}"

            if status.Found then
                match Wander.Main.runWithDefaults assertions with
                | Ok(Expression.Assertions assertions) -> Ok assertions
                | Ok x -> failwith $"OK - unexpected result - {x}"
                | Error err -> failwith err.UserMessage
            else
                error $"KB {name} not found." None

        member this.ReadDefinitionsKB(Term name) : Result<Definitions, LigatureError> =
            let f = new SimpleFunctions<string, string>()
            use session = db.NewSession f
            let status, definitions = session.Read $"D:{name}"

            if status.Found then
                match Wander.Main.runWithDefaults definitions with
                | Ok(Expression.Definitions definitions) -> Ok definitions
                | Ok x -> failwith $"OK - unexpected result - {x}"
                | Error err -> failwith err.UserMessage
            else
                error $"KB {name} not found." None

        member this.UnassertKB (Term name) (assertions: Assertions) =
            let f = new SimpleFunctions<string, string>()
            use session = db.NewSession f

            //TODO read current assertions
            let status, value = session.Read $"A:{name}"

            if status.Found then
                match Wander.Main.runWithDefaults value with
                | Ok(Expression.Assertions prevAssertions) ->
                    let newAssertions = Set.difference prevAssertions assertions
                    let value = printAny (Expression.Assertions newAssertions)
                    session.Upsert($"A:{name}", value) |> ignore
                    session.CompletePending() |> ignore
                | Ok x -> failwith $"Unexpected value - {x}"
                | Error err -> failwith err.UserMessage
            else
                failwith "TODO"

        member this.UndefineKB (Term name) (definitions: Definitions) : unit =
            let f = new SimpleFunctions<string, string>()
            use session = db.NewSession f

            //TODO read current assertions
            let status, value = session.Read $"D:{name}"

            if status.Found then
                match Wander.Main.runWithDefaults value with
                | Ok(Expression.Definitions prevDefinitions) ->
                    let newDefinitions = Set.difference prevDefinitions definitions
                    let value = printAny (Expression.Definitions newDefinitions)
                    session.Upsert($"D:{name}", value) |> ignore
                    session.CompletePending() |> ignore
                | Ok x -> failwith $"Unexpected value - {x}"
                | Error err -> failwith err.UserMessage
            else
                failwith "TODO"

        member this.IsConsistent(Term name) : Result<bool, LigatureError> =
            let definitions = (this :> ILigatureStore).ReadDefinitionsKB(Term name)
            let assertions = (this :> ILigatureStore).ReadAssertsKB(Term name)

            match definitions, assertions with
            | Ok definitions, Ok assertions -> isConsistent definitions assertions
            | _ -> failwith "TODO"

        member this.IsEquivalent (arg1: Term) (arg2: ConceptExpr) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.IsInstance (arg1: Term) (arg2: Instance) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.IsSatisfiable (arg1: Term) (arg2: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.IsSubsumedBy (arg1: Term) (arg2: ConceptExpr) (arg3: ConceptExpr) : Result<bool, LigatureError> =
            failwith "Not Implemented"

        member this.Query (arg1: Term) (arg2: ConceptExpr) : Result<Instance, LigatureError> =
            failwith "Not Implemented"

        member this.ReadKB(arg1: Term) : Result<KnowledgeBase, LigatureError> = failwith "Not Implemented"
        member this.TableauModels(arg1: Term) : Result<Set<Assertions>, LigatureError> = failwith "Not Implemented"

let inMemoryStore () : ILigatureStore =
    let config = new FasterKVSettings<string, string>(null)
    new LigatureStore(new FasterKV<string, string>(config))
