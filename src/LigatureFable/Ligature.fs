// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature

type ScriptResult =
  | String of string
  | Integer of int64
  | Nothing
//  | Identifier of Identifier
  | Boolean of bool
  | List
  | Tuple
  | Graph

let readScriptResult (input: string) =
    //TODO read nothing
    //TODO read integer
    //TODO read string
    //TODO read identifier
    //TODO read boolean
    //TODO read list
    //TODO read tuple
    //TODO read lig
    ScriptResult.List
