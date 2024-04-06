// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LMDB.Main

open Ligature

type LigatureLMDB() = //(datasource: string) =

    interface ILigature with
        member _.AllDatasets() = failwith ""
        member _.DatasetExists (Dataset dataset) = failwith ""

        member _.CreateDataset (Dataset dataset) = failwith ""

        member this.RemoveDataset (Dataset dataset) = failwith ""

        member _.AllStatements (Dataset dataset) = failwith ""

        member _.NewIdentifier dataset : Result<Identifier, LigatureError> = failwith "todo"//Guid.NewGuid().ToString() |> identifier

        member _.AddStatements (dataset: Dataset) (statements: Statement list) = failwith ""

        member _.RemoveStatements (dataset: Dataset) (statements: Statement list) : Result<unit, LigatureError> = failwith ""

        member _.Query dataset query = failwith "TODO"

        member _.Close() = failwith ""
