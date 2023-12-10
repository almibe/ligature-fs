
module Ligature.Wander.Fable

open Ligature.Wander.Main
open Ligature.Wander.Bindings

let execute script = run script (newBindings())

printfn "%A" (execute "\"hello\"")
