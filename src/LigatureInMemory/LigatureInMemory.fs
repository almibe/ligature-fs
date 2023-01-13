// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Ligature

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

type LigatureInMemory() =
    let datasets: Set<Dataset> ref = ref Set.empty
    let mutable isOpen = true
    interface Ligature with
        member this.AllDatasets ()  =
            Ok (Set.toArray datasets.Value) //Ok datasets.Value
        member this.DatasetExists dataset =
            todo
        member this.CreateDataset dataset = 
            lock this (fun () ->
                datasets.Value <- Set.add dataset datasets.Value
                Ok ()
            )
        member this.RemoveDataset dataset =
            lock this (fun () ->
                datasets.Value <- Set.remove dataset datasets.Value
                Ok ()
            )
        member this.Query dataset query = todo //-> int -> int = failwith "" //TODO this is wrong
        member this.Write dataset write = todo //-> int -> int = failwith "" //TODO this is wrong
        member this.Close () =
            lock this (fun () ->
                isOpen <- false
                datasets.Value <- Set.empty
                Ok ()            
            )
