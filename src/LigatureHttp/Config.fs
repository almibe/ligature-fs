// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Http.Config

open Ligature.Sqlite.Main

type Auth = | NoAuth

type Mode = | SingleUser

type Persistance = | Sqlite of connectionString: LigatureSqliteConfig

type Config =
    { url: string
      auth: Auth
      mode: Mode
      persistance: Persistance }

let readConfig () =
    { url = "http://localhost:4200"
      auth = NoAuth
      mode = SingleUser
      persistance = Sqlite InMemory }
