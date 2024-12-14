// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Display

open Ligature.Main
open Fable.Core.JsInterop
open Wander.Main
open Wander.Commands
open Wander.Model
open Wander.Lib

let displayText (script: string) (htmlElementId: string) =
    let text = 
        match run stdCommands (emptyVariables()) script with
        | Ok(Some(value)) -> prettyPrint value
        | _ -> "{}"
    let element = emitJsExpr () $"document.querySelector(htmlElementId)"
    element?replaceChildren ()
    let pre = emitJsExpr () "document.createElement('pre')"
    let code = emitJsExpr () "document.createElement('code')"
    pre?appendChild code
    code?textContent <- text
    element?appendChild pre
    ()
