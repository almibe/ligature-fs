import { runWithFns, ok, isConsistent, aBox, tBox, equivalent, concept, instance, and, not } from "./Library.fs.js";

let fns = new Map()

console.log(isConsistent(tBox(equivalent("A", "B")), aBox(instance("a", and("A", not("B"))))))

fns.set("echo", (args) => {
    console.log(args)
    return ok(args[0])
})

console.log(JSON.stringify(runWithFns(fns, "echo [test]")))
