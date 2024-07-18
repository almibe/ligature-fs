// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

let coreBindings = Ligature.Wander.Bindings.coreBindings

let bindFunction (hostFunction: Ligature.Wander.Model.HostFunction) (bindings: Ligature.Wander.Model.Bindings) =
    Ligature.Wander.Model.bindFunction hostFunction bindings

let run script bindings =
    Ligature.Wander.Main.run script bindings

let printResult result = Ligature.Wander.Main.printResult result
