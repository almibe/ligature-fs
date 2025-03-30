import { runWithFns, ok } from "./Library.fs.js";

let actions = new Map()

actions.set("echo", (args) => {
    console.log(args)
    return ok(args[0])
})

console.log(JSON.stringify(runWithFns(actions, "echo [test]")))
