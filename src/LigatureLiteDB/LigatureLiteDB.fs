// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LiteDB

open Ligature
open LiteDB

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

type LigatureLiteDB() =
    let db = new LiteDatabase(":memory:")
    let mutable isOpen = true
    interface Ligature with
        member this.AllDatasets ()  =
            todo
        member this.DatasetExists dataset =
            todo
        member this.CreateDataset dataset = 
            todo
        member this.RemoveDataset dataset =
            todo
        member this.Query dataset query = todo
        member this.Write dataset write = todo
        member this.Close () =
            todo
