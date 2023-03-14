// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use execContext = Fake.Core.Context.FakeExecutionContext.Create false "path/to/script.fsx" []
Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)

open Fake
open Fake.Core
open Fake.JavaScript

Target.create "RunLigatureLab" (fun _ ->
    Npm.install (fun o ->
        { o with
            WorkingDirectory = "./src/LigatureLabJS"
        })
    Npm.run "build" (fun o ->
        { o with
            WorkingDirectory = "./src/LigatureLabJS"
        }
    )
    System.IO.File.Copy("./src/LigatureLabJS/public/bundle.js", "./src/LigatureLab/WebRoot/bundle.js")
)

Target.runOrDefault "RunLigatureLab"
