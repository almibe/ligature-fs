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

type Slot = Slot of string

type ResultSet = Set<ValueSet>

and ValueSet = Map<Slot, Value>

and [<RequireQualifiedAccess>] ElementPattern =
    | Element of Element
    | Slot of Slot

and [<RequireQualifiedAccess>] Value =
    | Element of Element
    | Literal of string
    | Slot of Slot

and Triple = ElementPattern * ElementPattern * Value

and Network = Set<Triple>
