// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.TinyDL

open Ligature.Model
open Wander.Model
open Wander.Interpreter

let rec infer (tBox: Network) (aBox: Network) : Result<Network, LigatureError> =
    let mutable res = aBox

    Set.iter
        (fun tStatement ->
            Set.iter
                (fun aStatement ->
                    match (tStatement, aStatement) with
                    | (ElementPattern.Element subconcept,
                       ElementPattern.Element(Element "subconcept-of"),
                       Value.Element superconcept),
                      (ElementPattern.Element element, ElementPattern.Element(Element ":"), Value.Element concept) when
                        subconcept = concept
                        ->
                        res <-
                            Set.add
                                (ElementPattern.Element element,
                                 ElementPattern.Element(Element ":"),
                                 Value.Element superconcept)
                                res
                    | (ElementPattern.Element firstRole,
                       ElementPattern.Element(Element "tdl.inverse-of"),
                       Value.Element secondRole),
                      (ElementPattern.Element first, ElementPattern.Element role, Value.Element second) when
                        role = firstRole
                        ->
                        res <-
                            Set.add
                                (ElementPattern.Element second, ElementPattern.Element secondRole, Value.Element first)
                                res
                    | (ElementPattern.Element firstRole,
                       ElementPattern.Element(Element "tdl.inverse-of"),
                       Value.Element secondRole),
                      (ElementPattern.Element first, ElementPattern.Element role, Value.Element second) when
                        role = secondRole
                        ->
                        res <-
                            Set.add
                                (ElementPattern.Element second, ElementPattern.Element firstRole, Value.Element first)
                                res
                    | (ElementPattern.Element roleName,
                       ElementPattern.Element(Element ":"),
                       Value.Element(Element "tdl.Is-Symmetrical")),
                      (ElementPattern.Element first, ElementPattern.Element role, Value.Element second) when
                        role = roleName
                        ->
                        res <-
                            Set.add
                                (ElementPattern.Element second, ElementPattern.Element role, Value.Element first)
                                res
                    | _ -> ())
                aBox)
        tBox

    if aBox = res then Ok res else infer tBox res

let inferAction: Action =
    Action.Full(
        { doc = "..."; examples = [] },
        fun _ networks stack ->
            match stack with
            | description :: network :: tail ->
                let description =
                    match description with
                    | Any.Network n -> n
                    | Any.NetworkName name ->
                        match networks.TryFind name with
                        | Some(n) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let network =
                    match network with
                    | Any.Network n -> n
                    | Any.NetworkName name ->
                        match networks.TryFind name with
                        | Some(n) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                match infer description network with
                | Ok res -> Ok(networks, Any.Network res :: tail)
                | Error err -> error $"Error calling infer: {err}" None
            | _ -> error "Improper call to infer." None
    )
