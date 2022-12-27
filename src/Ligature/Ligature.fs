// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature

type Identifier = private Identifier of System.String

//TODO doesn't validate/should return Result type
let identifier id = Identifier id

let readIdentifier (identifier: Identifier) =
    match identifier with
    | Identifier(id) -> id

//type Value =
