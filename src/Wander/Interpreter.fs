// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model

let rec evalScript
    (actions: Actions)
    (networks: Networks)
    (stack: Stack)
    (script: Script)
    : Result<Networks * Stack, LigatureError> =
    match script with
    | [] -> Ok(networks, stack)
    | head :: tail ->
        match head with
        | Any.Element action ->
            match executeAction actions networks stack action with
            | Ok(networks, stack) -> evalScript actions networks stack tail
            | Error err -> Error err
        | value -> evalScript actions networks (value :: stack) tail

and createAction (quote: Quote) : Action =
    Action.Full(fun actions networks stack -> evalScript actions networks stack quote)

and lookupAction (actions: Actions) (networks: Networks) (action: Element) : Action option =

    let actionInNetwork: Action option =
        match Map.tryFind (NetworkName "*") networks with
        | Some(baseNetwork) ->
            let res: Action seq =
                Seq.choose
                    (fun triple ->
                        match triple with
                        | ElementPattern.Element el, ElementPattern.Element(Element "action-def"), Value.Quote def ->
                            if el = action then Some(createAction def) else None
                        | _ -> None)
                    baseNetwork

            if Seq.length res = 1 then
                (List.ofSeq res).Head |> Some
            else
                None
        | None -> None

    if Option.isNone actionInNetwork then
        match Map.tryFind action actions with
        | Some(action) -> Some(action)
        | None -> None
    else
        actionInNetwork

and executeAction (actions: Actions) (networks: Networks) (stack: Stack) (action: Element) =
    match lookupAction actions networks action with
    | Some(Action.Full action) -> action actions networks stack
    | Some(Action.Stack action) ->
        match action stack with
        | Ok stack -> Ok(networks, stack)
        | Error err -> Error err
    | None -> error $"Could not find action {action}." None
