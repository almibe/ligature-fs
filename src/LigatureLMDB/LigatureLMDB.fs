// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LMDB.Main

open Ligature

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

type LigatureLMDB() =
    member this.initialize() = ()

    interface ILigature with
        member this.AllDatasets() = Ok []
        member this.DatasetExists dataset = todo
        member this.CreateDataset dataset = todo
        member this.RemoveDataset dataset = todo
        member this.Query dataset query = todo
        member this.Write dataset write = todo
        member this.Close() = todo
