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

type Variable = Variable of string

type ResultSet = Set<ValueSet>

and ValueSet = Map<Variable, ElementPattern>

and [<RequireQualifiedAccess>] ElementPattern =
    | Element of Element
    | Variable of Variable

and Triple = ElementPattern * ElementPattern * ElementPattern

and Network = Set<Triple>

and Quote = Any list

and AnySet = Set<Any>

and [<RequireQualifiedAccess>] Any =
    | Variable of Variable
    | Quote of Quote
    | Literal of string
    | Element of Element
    | Network of Network
    | ValueSet of ValueSet
    | ResultSet of ResultSet
    | Comment of string
    | AnySet of AnySet
