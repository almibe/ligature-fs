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

// module Element =
//   let isValid e = failwith "TODO"

//   let create (e: string) =
//     if isValid e then
//       Some (Element e)
//     else
//       None

//   let value (Element e) = e

type ConceptName = Element

type RoleName = Element

type Quote = Value list

and [<RequireQualifiedAccess>] Value = 
  | Value of string
  | Quote of Quote
  | Element of Element
  | Network of Network

and Extends =
    { element: Element
      concept: ConceptName }

and NotExtends =
    { element: Element
      concept: ConceptName }

and Attribute =
    { element: Element
      attribute: Element
      value: Value }

and [<RequireQualifiedAccess>] Entry =
    | Extends of Extends
    | NotExtends of NotExtends
    | Attribute of Attribute

and Network = Set<Entry>

and NetworkName = string

type LigatureEngine =
    inherit System.IDisposable
    abstract Networks: unit -> Result<Set<NetworkName>, LigatureError>
    abstract ReadNetwork: NetworkName -> Result<Network, LigatureError>
    abstract AddNetwork: NetworkName -> Result<unit, LigatureError>
    abstract RemoveNetwork: NetworkName -> Result<unit, LigatureError>
    abstract AddEntries: NetworkName -> Network -> Result<unit, LigatureError>
    abstract SetNetwork: NetworkName -> Network -> Result<unit, LigatureError>
    abstract RemoveEntries: NetworkName -> Network -> Result<unit, LigatureError>
    abstract FilterEntries: NetworkName -> Network -> Result<Network, LigatureError>
