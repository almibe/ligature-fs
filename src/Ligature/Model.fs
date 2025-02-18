// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Model

type LigatureError =
    { UserMessage: string
      DebugMessage: string option }

let error userMessage debugMessage =
    Error(
        { UserMessage = userMessage
          DebugMessage = debugMessage }
    )

type Term = Term of string

type Slot = Slot of string

type ResultSet = Set<ValueSet>

and ValueSet = Map<Slot, TermPattern>

and [<RequireQualifiedAccess>] TermPattern =
    | Term of Term
    | Slot of Slot

and Triple = Term * Term * Term

and Network = Set<Triple>

and TriplePattern = TermPattern * TermPattern * TermPattern

and Pattern = Set<TriplePattern>

type INetwork =
    abstract Triples: unit -> Async<Network>

type ILigatureStore =
    abstract Networks: unit -> Async<string seq>
    abstract AddNetwork: string -> Async<unit>
    abstract RemoveNetwork: string -> Async<unit>
    abstract Read: string -> Async<INetwork>
