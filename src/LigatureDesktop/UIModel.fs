// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

namespace LigatureDesktop

open Ligature
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

    /// This function initializes and returns an instance of UIModel.
    /// Initialization includes querying all Datasets in the instance and
    /// Selecting the first Dataset as the default.
    let initializeModel () = 
        let ligature = ligatureSqlite (defaultFile ())
        let (output, selectedDataset, datasets) = 
            match ligature.AllDatasets () with
            | Ok(datasets) ->
                let datasets = List.map (fun ds -> readDataset ds) datasets
                ("", List.head datasets, datasets)
            | Error(err) -> ($"Error\n{err.userMessage}\n{err.debugMessage}", "", [])

        {
            Datasets = datasets
            Output = output
            Ligature = ligature
            NewDataset = ""
            SelectedDataset = selectedDataset
        }
