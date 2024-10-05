// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Infer.Test

open Expecto
open Ligature.Main
open Ligature.InMemoryStore
open TinyDL.NewParser
open TinyDL.Interpreter

let unsafeRead (input: string) =
    match read input with
    | Ok(nodes) ->
        match express nodes with
        | Ok kb -> failwith "TODO"
        // match normalize kb with
        // | Ok res -> res
        // | Error err -> failwith err
        | Error err -> failwith err
    | Error err -> failwith err

[<Tests>]
let evalTests =
    testList
        "Eval Tests"
        [ testCase "Eval empty KB" <| fun _ -> Expect.equal (eval "") (Ok(Set.empty)) ""
          testCase "Normalize simple ABox"
          <| fun _ ->
              Expect.equal
                  (unsafeRead "a: B, c: D, e: ¬F")
                  (Set.ofList
                      [ Extension(
                            { element = Symbol "a"
                              concept = Symbol "B" }
                        )
                        Extension(
                            { element = Symbol "c"
                              concept = Symbol "D" }
                        )
                        NonExtension(
                            { element = Symbol "e"
                              concept = Symbol "F" }
                        ) ])
                  "" ]

[<Tests>]
let tests =
    testList
        "Consistency Tests"
        [ testCase "Check empty ABox for consistency"
          <| fun _ -> Expect.equal (isConsistent (Set.empty)) true ""
          testCase "Check simple ABox for consistency"
          <| fun _ ->
              Expect.equal
                  (isConsistent (
                      Set.ofList
                          [ (Extension
                                { element = Symbol "a"
                                  concept = Symbol "B" }) ]
                  ))
                  true
                  ""
          //          <| fun _ -> Expect.equal (consistent (unsafeRead "a: B, c: D, e: ¬F")) (Ok(true)) ""
          testCase "Check simple ABox for inconsistency"
          <| fun _ ->
              Expect.equal
                  (isConsistent (
                      Set.ofList
                          [ (Extension
                                { element = Symbol "a"
                                  concept = Symbol "B" })
                            (NonExtension
                                { element = Symbol "a"
                                  concept = Symbol "B" }) ]
                  ))
                  false
                  "" ]
//   testCase "Check ABox for consistency with conjunctions"
//   <| fun _ -> Expect.equal (consistent (unsafeRead "a: B ⊓ ¬D, c: D, a: ¬A ⊓ ¬B ⊓ ¬C")) (Ok(false)) "" ]

// testCase "Call interpret on empty arguments"
// <| fun _ ->
//     Expect.equal
//         (interpret emptyKB)
//         (Ok
//             { Domain = Set.empty
//               Concepts = Map.empty
//               Roles = Map.empty })
//         ""
//   testCase "Call interpret with empty TBox"
//   <| fun _ ->
//       Expect.equal
//           (interpret (Set.empty, (Set.ofList [ UnaryPredicate { symbol = "betty"; concept = AtomicConcept "Cat" } ])))
//           (Ok
//               { Domain = Set.ofList [ Symbol "betty" ]
//                 Concepts = Map.ofList [ (Symbol "Cat", Set.ofList [ Symbol "betty" ]) ]
//                 Roles = Map.empty })
//           ""
// testCase "Call interpret with empty ABox"
// <| fun _ ->
//     Expect.equal
//         (interpret (
//             (Set.ofList
//                 [ Definition
//                       { left = "DomesticCat"
//                         right = AtomicConcept "HouseCat" } ]),
//             emptyABox
//         ))
//         (Ok
//             { Domain = Set.ofList []
//               Concepts = Map.ofList [ (Symbol "DomesticCat", Set.empty); (Symbol "HouseCat", Set.empty) ]
//               Roles = Map.empty })
//         ""
//   testCase "Read multiple unary predicates with one individual"
//   <| fun _ ->
//       Expect.equal
//           (eval "betty: Cat, betty: HouseCat")
//           (Ok
//               { Domain = Set.ofList [ Symbol "betty" ]
//                 Concepts =
//                   Map.ofList
//                       [ (Symbol "Cat", Set.ofList [ Symbol "betty" ])
//                         (Symbol "HouseCat", Set.ofList [ Symbol "betty" ]) ]
//                 Roles = Map.empty })
//           ""
//   testCase "Read multiple unary predicates"
//   <| fun _ ->
//       Expect.equal
//           (eval "betty: Cat, don: Cat")
//           (Ok
//               { Domain = Set.ofList [ Symbol "betty"; Symbol "don" ]
//                 Concepts = Map.ofList [ (Symbol "Cat", Set.ofList [ Symbol "betty"; Symbol "don" ]) ]
//                 Roles = Map.empty })
//           ""
//   testCase "Binary predicate"
//   <| fun _ ->
//       Expect.equal
//           (eval "(betty, 11lbs): weight")
//           (Ok
//               { Domain = Set.ofList [ Symbol "betty"; Symbol "11lbs" ]
//                 Concepts = Map.empty
//                 Roles = Map.ofList [ ("weight", Set.ofList [ ("betty", "11lbs") ]) ] })
//           "" ]
// testCase "Basic Definition"
// <| fun _ ->
//     Expect.equal
//         (eval "Cat ≡ HouseCat, betty: Cat")
//         (Ok
//             { Domain = Set.ofList [ Symbol "betty" ]
//               Concepts =
//                 Map.ofList
//                     [ (Symbol "Cat", Set.ofList [ Symbol "betty" ])
//                       (Symbol "HouseCat", Set.ofList [ Symbol "betty" ]) ]
//               Roles = Map.empty })
//         "" ]
// testCase "Basic Inclusion"
// <| fun _ ->
//     Expect.equal
//         (eval "Cat ⊑ Animal, betty: Cat")
//         (Ok
//             { Domain = Set.ofList [ Symbol "betty" ]
//               Concepts =
//                 Map.ofList
//                     [ (Symbol "Cat", Set.ofList [ Symbol "betty" ])
//                       (Symbol "Animal", Set.ofList [ Symbol "betty" ]) ]
//               Roles = Map.empty })
//         ""
// testCase "Basic Existential Restriction"
// <| fun _ ->
//     Expect.equal
//         (eval "Cat ⊑ ∃weight.Weight, (betty, 11lbs): weight")
//         (Ok
//             { Domain = Set.ofList [ Symbol "betty"; Symbol "11lbs" ]
//               Concepts =
//                 Map.ofList
//                     [ (Symbol "Cat", Set.ofList [ Symbol "betty" ])
//                       (Symbol "Weight", Set.ofList [ Symbol "11lbs" ]) ]
//               Roles = Map.ofList [ (Symbol "weight", Set.ofList [ (Symbol "betty", Symbol "11lbs") ]) ] })
//         "" ]
