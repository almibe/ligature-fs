// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Store

open Ligature.Model
open Wander.InMemoryStore
open Mikodev.Binary
open FASTER.core
open System

type Event = int * string * string * string * int * string

let valueToType value =
    match value with
    | Value.Term _ -> 0
    | Value.Literal _ -> 1

let valueToString value =
    match value with
    | Value.Term(Term t) -> t
    | Value.Literal(Literal l) -> l

type LigatureStore(path: string option) =
    let e_add = 0
    let e_remove = 1
    let e_assert = 2
    let e_define = 3
    let e_unassert = 4
    let e_undefine = 5

    let generator = Generator.CreateDefault()

    let path =
        match path with
        | None -> null
        | Some value -> value

    let config = new FasterLogSettings(path)
    let log = new FasterLog(config)

    let store: ILigatureStore = InMemoryStore()

    member _.Init() =
        use iter = log.Scan(log.BeginAddress, Int64.MaxValue)
        let mutable cont = true

        while cont do
            let succ, value, _, _ = iter.GetNext()

            if succ then
                match generator.Decode<Event> value with
                | 0, name, _, _, _, _ -> store.AddKnowledgeBase name
                | 1, name, _, _, _, _ -> store.RemoveKnowledgeBase name
                | 2, name, e, r, 0, v ->
                    store.AssertKnowledgeBase name (Set.ofList [ Term e, Term r, Value.Term(Term v) ])
                | 2, name, e, r, 1, v ->
                    store.AssertKnowledgeBase name (Set.ofList [ Term e, Term r, Value.Literal(Literal v) ])
                | 3, name, e, r, 0, v ->
                    store.DefineKnowledgeBase name (Set.ofList [ Term e, Term r, Value.Term(Term v) ])
                | 3, name, e, r, 1, v ->
                    store.DefineKnowledgeBase name (Set.ofList [ Term e, Term r, Value.Literal(Literal v) ])
                | 4, name, e, r, 0, v ->
                    store.UnassertKnowledgeBase name (Set.ofList [ Term e, Term r, Value.Term(Term v) ])
                | 4, name, e, r, 1, v ->
                    store.UnassertKnowledgeBase name (Set.ofList [ Term e, Term r, Value.Literal(Literal v) ])
                | 5, name, e, r, 0, v ->
                    store.UndefineKnowledgeBase name (Set.ofList [ Term e, Term r, Value.Term(Term v) ])
                | 5, name, e, r, 1, v ->
                    store.UndefineKnowledgeBase name (Set.ofList [ Term e, Term r, Value.Literal(Literal v) ])
                | _ -> failwith "Unexpected event type."
            else
                cont <- false

    interface IDisposable with
        member _.Dispose() : unit =
            config.Dispose()
            log.Dispose()

    interface ILigatureStore with
        member this.KnowledgeBases() : string seq = store.KnowledgeBases()

        member this.AddKnowledgeBase(name: string) : unit =
            store.AddKnowledgeBase name
            let event: Event = e_add, name, "", "", 0, ""
            let encoded = generator.Encode event
            log.Enqueue encoded |> ignore
            log.Commit()

        member this.RemoveKnowledgeBase(name: string) : unit =
            store.RemoveKnowledgeBase name
            let event: Event = e_remove, name, "", "", 0, ""
            let encoded = generator.Encode event
            log.Enqueue encoded |> ignore
            log.Commit()

        member this.AssertKnowledgeBase (name: string) (network: Network) : unit =
            store.AssertKnowledgeBase name network

            Set.iter
                (fun (Term e, Term r, v) ->
                    let event: Event = e_assert, name, e, r, valueToType v, valueToString v
                    let encoded = generator.Encode event
                    log.Enqueue encoded |> ignore)
                network

            log.Commit()

        member this.DefineKnowledgeBase (name: string) (network: Network) : unit =
            store.DefineKnowledgeBase name network

            Set.iter
                (fun (Term e, Term r, v) ->
                    let event: Event = e_define, name, e, r, valueToType v, valueToString v
                    let encoded = generator.Encode event
                    log.Enqueue encoded |> ignore)
                network

            log.Commit()

        member this.Read(name: string) : Result<Network, LigatureError> = store.Read name

        member this.ReadAsserts(name: string) : Result<Network, LigatureError> = store.ReadAsserts name

        member this.ReadDefinitions(name: string) : Result<Network, LigatureError> = store.ReadDefinitions name

        member this.UnassertKnowledgeBase (name: string) (network: Network) : unit =
            store.UnassertKnowledgeBase name network

            Set.iter
                (fun (Term e, Term r, v) ->
                    let event: Event = e_unassert, name, e, r, valueToType v, valueToString v
                    let encoded = generator.Encode event
                    log.Enqueue encoded |> ignore)
                network

            log.Commit()

        member this.UndefineKnowledgeBase (name: string) (network: Network) : unit =
            store.UndefineKnowledgeBase name network

            Set.iter
                (fun (Term e, Term r, v) ->
                    let event: Event = e_undefine, name, e, r, valueToType v, valueToString v
                    let encoded = generator.Encode event
                    log.Enqueue encoded |> ignore)
                network

            log.Commit()
