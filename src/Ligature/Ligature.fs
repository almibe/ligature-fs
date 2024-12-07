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

type Element = Element of string

type ConceptName = Element

type RoleName = Element

type Value = Value of string

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

type Attribute =
    { element: Element
      attribute: Element
      value: Value}

[<RequireQualifiedAccess>]
type Entry =
    | Extends of Extends
    | NotExtends of NotExtends
    | Role of Role
    | Attribute of Attribute

type Network = Set<Entry>

type NetworkName = string

type LigatureStore =
    inherit System.IDisposable
    abstract Networks: unit -> Result<Set<NetworkName>, LigatureError>
    abstract ReadNetwork: NetworkName -> Result<Network, LigatureError>
    abstract AddNetwork: NetworkName -> Result<unit, LigatureError>
    abstract RemoveNetwork: NetworkName -> Result<unit, LigatureError>
    abstract AddEntries: NetworkName -> Network -> Result<unit, LigatureError>
    abstract SetNetwork: NetworkName -> Network -> Result<unit, LigatureError>
    abstract RemoveEntries: NetworkName -> Network -> Result<unit, LigatureError>
    abstract FilterEntries: NetworkName -> Network -> Result<Network, LigatureError>
