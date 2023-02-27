// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

namespace LigatureDesktop

open Ligature
open Avalonia.FuncUI
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Ligature.Sqlite.Main

module Model =
    type UIModel = {
        Ligature: Ligature
        Datasets: string list
        SelectedDataset: string
        Output: string
        NewDataset: string
    }

    let defaultFile () =
        File "test.sqlite3"


    let createModel () = { 
        Datasets = []
        Output = ""
        Ligature = ligatureSqlite (defaultFile ()) //InMemory// :> Ligature
        NewDataset = ""
        SelectedDataset = ""
        }

