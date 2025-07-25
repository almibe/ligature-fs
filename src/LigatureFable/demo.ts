// import { run } from "./model.js"
//import { runWithFns } from "./dist/ligature.es"

import { runWithFns } from "./Library.fs.js"

let fns = new Map()

fns.set("test", (assertions) => console.log("Hello " + assertions.assertions.size))

runWithFns(
    fns,
    "test(assertions(triple(a b c)))"
)
