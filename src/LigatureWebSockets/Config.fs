// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.WebSockets.Config

open Argu
open System

type Config = { url: string }

let readConfig () = { url = "http://localhost:4200" }
