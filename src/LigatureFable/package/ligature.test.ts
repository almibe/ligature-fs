import { run } from "./lib/ligature.js"
// import { glob } from "glob"
// import fs from 'node:fs'
import { expect, test } from 'vitest'
// import { stateToJS } from "../Ligature.fs.js"

// const dir = process.env.LIGATURE_TEST_SUITE + "/wander/*.wander"
// const wanderFiles = await glob(dir)

// for (const file of wanderFiles) {
//     test(`Testing ${file.replace("\\", "/").split("/").at(-1)}`, () => {
//         const script = fs.readFileSync(file, 'utf8')
//         const res = run(script)
//         expect(res.error).toEqual(undefined)
//         expect(res.length).toEqual(0)
//     })
// }

test("Eval Named Network With Role", () => {
    expect(run("set test {a b c}, read test"))
        .toEqual(
                {"test": [{type: "role", first: {symbol: "a"}, second: { symbol: "c"}, role: {symbol: "b"}}]})
})

test("Eval Named Network With Extension", () => {
    expect(run("set test {betty : Cat}, read test"))
        .toEqual( 
                {"test": [{type: "extension", element: {symbol: "betty"}, concept: { symbol: "Cat"}}]})
})

// test("Call Function", () => {
//     expect(run("count {`a` `b` `c`}")).toEqual(1n)
// })

// test("Add Basic HostFunction from JS", () => {
//     const testFunction: WanderFunction = {
//         module: "test",
//         name: "test",
//         description: "test",
//         eval: (args) => "test",
//     }

//     const newBindings = bindFunction(testFunction, coreBindings)
//     expect(run("test ()", newBindings)).toEqual("test")
// })

// test("Add Basic HostFunction from JS That Uses Args", () => {
//     const testFunction: WanderFunction = {
//         module: "test",
//         name: "add",
//         description: "test",
//         eval: (args) => {
//             return args[0] + args[1]
//         },
//     }

//     const newBindings = bindFunction(testFunction, coreBindings)
//     expect(run("add 1 2", newBindings)).toEqual(3n)
// })
