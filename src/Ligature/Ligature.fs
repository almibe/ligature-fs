// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Main

type LigatureError =
    { UserMessage: string
      DebugMessage: string option }

let error userMessage debugMessage =
    Error(
        { UserMessage = userMessage
          DebugMessage = debugMessage }
    )

type Symbol = Symbol of string

type Element = Symbol

type ConceptName = Symbol

type RoleName = Symbol

type Extends =
    { element: Element
      concept: ConceptName }

type NotExtends =
    { element: Element
      concept: ConceptName }

type Role =
    { first: Element
      second: Element
      role: RoleName }

[<RequireQualifiedAccess>]
type Entry =
    | Extends of Extends
    | NotExtends of NotExtends
    | Role of Role

type Network = Set<Entry>

type NetworkName = string

type LigatureStore =
    abstract Networks: unit -> Result<Set<NetworkName>, LigatureError>
    abstract Read: NetworkName -> Result<Network, LigatureError>
    abstract AddNetwork: NetworkName -> Result<unit, LigatureError>
    abstract RemoveNetwork: NetworkName -> Result<unit, LigatureError>
    abstract ClearNetwork: NetworkName -> Result<unit, LigatureError>
    abstract Add: NetworkName -> Network -> Result<unit, LigatureError>
    abstract Set: NetworkName -> Network -> Result<unit, LigatureError>
    abstract Remove: NetworkName -> Network -> Result<unit, LigatureError>
    abstract Filter: NetworkName -> Network -> Result<Network, LigatureError>

let printSymbol (Symbol(symbol)) : string = symbol
