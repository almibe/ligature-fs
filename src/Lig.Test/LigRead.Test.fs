// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module LigTestRead

open Expecto
open Ligature
open Ligature.Lig.Read

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let unwrap result =
    Result.defaultWith (fun _ -> todo) result

let statement (entity: string) (attribute: string) (value: Value) =
    let entity = unwrap (identifier entity)
    let attribute = unwrap (identifier attribute)

    let statement: Statement =
        { Entity = entity
          Attribute = attribute
          Value = value }

    statement

[<Tests>]
let tests =
    testList
        "Lig Read Suite"
        [ testCase "read Identifiers"
          <| fun _ -> Expect.equal (readIdentifier (Gaze.fromString "<a>")) (identifier "a") ""

          testCase "read Values"
          <| fun _ -> Expect.equal (readValue (Gaze.fromString "<a>")) (Ok(Identifier(unwrap (identifier "a")))) ""
          //TODO fix below
          //Expect.equal (readValue (Gaze.fromString "4321")) (Ok(Integer(4321L))) ""
          //Expect.equal (readValue (Gaze.fromString "\"hello\"")) (Ok(String("hello"))) ""
          //assertEq(readValue(Gaze.fromString("0xDEAD")), Ok(unwrap(identifier("a"))))

          testCase "read single Statement"
          <| fun _ ->
              Expect.equal
                  (readLig "<a> <b> <c>")
                  (Ok([ (statement "a" "b" (Identifier(unwrap (identifier ("c"))))) ]))
                  "" ]
