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

type NetworkName = NetworkName of string

type Variable = Variable of string

type ResultSet = Set<ValueSet>

and ValueSet = Map<Variable, Value>

and [<RequireQualifiedAccess>] ElementPattern =
    | Element of Element
    | Variable of Variable

and [<RequireQualifiedAccess>] Value =
    | Element of Element
    | Literal of string
    | Variable of Variable
    | Quote of Quote
    | NetworkName of NetworkName

and Triple = ElementPattern * ElementPattern * Value

and Network = Set<Triple>

and Networks = Map<Element, Network>

and Quote = Any list

and [<RequireQualifiedAccess>] Any =
    | Literal of string
    | Variable of Variable
    | Quote of Quote
    | Element of Element
    | Network of Network
    | ValueSet of ValueSet
    | ResultSet of ResultSet
    | NetworkName of NetworkName
