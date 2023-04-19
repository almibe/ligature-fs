// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature

let run script =
    match Ligature.Wander.Main.run script (Ligature.Wander.Preludes.standardPrelude ()) with
    | Ok(result) -> Ligature.Wander.Model.prettyPrint result
    | Error(err) -> err.UserMessage
