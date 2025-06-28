// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Store

open Ligature.Model

type LigatureStore(path: string option) =

    let path =
        match path with
        | None -> null
        | Some value -> value

    interface ILigatureStore with
        member this.AddKB(arg1: Term) : unit = failwith "Not Implemented"
        member this.AssertKB (arg1: Term) (arg2: Assertions) : unit = failwith "Not Implemented"
        member this.DefineKB (arg1: Term) (arg2: Definitions) : unit = failwith "Not Implemented"
        member this.KBs() : Term seq = failwith "Not Implemented"
        member this.ReadAssertsKB(arg1: Term) : Result<Assertions, LigatureError> = failwith "Not Implemented"
        member this.ReadDefinitionsKB(arg1: Term) : Result<Definitions, LigatureError> = failwith "Not Implemented"
        member this.RemoveKB(arg1: Term) : unit = failwith "Not Implemented"
        member this.UnassertKB (arg1: Term) (arg2: Assertions) : unit = failwith "Not Implemented"
        member this.UndefineKB (arg1: Term) (arg2: Definitions) : unit = failwith "Not Implemented"
