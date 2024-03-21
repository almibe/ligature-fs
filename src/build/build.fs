// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

let execContext =
    Fake.Core.Context.FakeExecutionContext.Create false "path/to/script.fsx" []

Fake.Core.Context.setExecutionContext (Fake.Core.Context.RuntimeContext.Fake execContext)

open Fake
open Fake.Core
open Fake.DotNet

Target.create "RunAllTests" (fun tp ->
    let testProjects =
        [ "./src/Gaze.Test/Gaze.Test.fsproj"
          "./src/Lig.Test/Lig.Test.fsproj"
          "./src/Ligature.Test/Ligature.Test.fsproj"
          "./src/LigatureInMemory.Test/LigatureInMemory.Test.fsproj"
          "./src/LigatureLab.Test/LigatureLab.Test.fsproj"
          "./src/LigatureLMDB.Test/LigatureLMDB.Test.fsproj"
          "./src/LigatureSqlite.Test/LigatureSqlite.Test.fsproj"
          "./src/Bend.Test/Bend.Test.fsproj" ]

    let failed =
        testProjects
        |> List.filter (fun proj ->
            let res = DotNet.exec (fun o -> o) "run" $"--project {proj}"
            not res.OK)

    if not failed.IsEmpty then
        printfn "Tests failed in:"
        List.iter (fun proj -> printfn $" - {proj}") failed
        System.Environment.Exit(-1))

Target.runOrDefault "RunAllTests"
