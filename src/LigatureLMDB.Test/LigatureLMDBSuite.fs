// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LMDB.Test.Suite

open Expecto
open Ligature.Main
open Ligature.LMDB
open System.IO

let testLocation =
    Path.GetTempPath()
    + Path.DirectorySeparatorChar.ToString()
    + "ligatureLMDB"
    + System.Guid.NewGuid().ToString()
    + Path.DirectorySeparatorChar.ToString()

let newTestInstance () : LigatureStore =
    let directory = DirectoryInfo(testLocation)

    if directory.Exists then
        directory.Delete(true)

    directory.Create()
    openStore testLocation

[<Tests>]
let tests =
    testList
        "LMDB Store"
        [ testCase "Start with no networks."
          <| fun _ ->
              use store = newTestInstance ()
              Expect.equal (store.Networks()) (Ok(Set.empty)) ""
          testCase "Add network."
          <| fun _ ->
              use store = newTestInstance ()
              store.AddNetwork(NetworkName "test")
              Expect.equal (store.Networks()) (Ok(Set.ofList [ NetworkName "test" ])) ""
          testCase "Remove network."
          <| fun _ ->
              use store = newTestInstance ()
              store.AddNetwork(NetworkName "test")
              store.AddNetwork(NetworkName "test2")
              store.AddNetwork(NetworkName "test3")
              store.RemoveNetwork(NetworkName "test2")

              Expect.equal (store.Networks()) (Ok(Set.ofList [ NetworkName "test"; NetworkName "test3" ])) "" ]
