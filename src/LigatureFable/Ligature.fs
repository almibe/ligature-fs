// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open FSharp.Json

let run script network = Ligature.Wander.Main.run script network |> Json.serialize 

let printResult result = Ligature.Wander.Main.printResult result |> Json.serialize 
