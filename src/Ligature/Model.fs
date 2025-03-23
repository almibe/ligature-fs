// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Model

type LigatureError =
    { UserMessage: string
      DebugMessage: string option }

let error userMessage debugMessage =
    Error
        { UserMessage = userMessage
          DebugMessage = debugMessage }

type Term = Term of string

type Literal = Literal of string

[<RequireQualifiedAccess>]
type Value =
    | Term of Term
    | Literal of Literal

type Slot = Slot of string

type ResultSet = Set<ValueSet>

and ValueSet = Map<Slot, Value>

and [<RequireQualifiedAccess>] TermPattern =
    | Term of Term
    | Slot of Slot

and [<RequireQualifiedAccess>] ValuePattern =
    | Term of Term
    | Literal of Literal
    | Slot of Slot

and Triple = Term * Term * Value

and Network = Set<Triple>

and TriplePattern = TermPattern * TermPattern * ValuePattern

and Pattern = Set<TriplePattern>

// type INetwork =
//     abstract Triples: unit -> Async<Network>

type ILigatureStore =
    abstract Stores: unit -> string seq
    abstract AddStore: string -> unit
    abstract RemoveStore: string -> unit
    abstract AssertStore: string -> Network -> unit
    abstract UnassertStore: string -> Network -> unit
    abstract ReadAsserts: string -> Result<Network, LigatureError>
