// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Http.Config

type Auth = | NoAuth

type Mode = | SingleUser

type Config =
    { url: string
      auth: Auth
      mode: Mode
      sqliteConnectionString: string }

let readConfig () =
    { url = "http://localhost:5000"
      auth = NoAuth
      mode = SingleUser
      sqliteConnectionString = "" }
