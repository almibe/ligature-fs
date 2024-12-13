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

type Variable = Variable of string

type Quote = Value list

and [<RequireQualifiedAccess>] Value =
    | Literal of string
    | Variable of Variable
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
