// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Serialization

open Main
open InMemoryNetwork
// open Wander.Lexer
// open FsToolkit.ErrorHandling
// open Wander.Model
// open Wander.Nibblers
// open System.Collections
// open System.IO
// open Wander.Main
// open Wander.Bindings

let readLigature (input: string): Result<INetwork, LigatureError> =
    Ok(emptyNetwork)

let writeLigature (input: INetwork): string =
    failwith "TODO"
    // Map.iter (fun (DatasetName dataset) statements ->
    //     writer.Write(prettyPrint (WanderValue.String dataset))
    //     writer.WriteLine()

    //     Set.iter
    //         (fun statement ->
    //             writer.Write(printStatement statement)
    //             writer.WriteLine())
    //         statements)
