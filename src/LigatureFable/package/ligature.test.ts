import { newEngine } from "./lib/ligature.js"
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

test("Empty Network", () => {
    let engine = newEngine()
    expect(engine.run("{}")).toEqual({})
})

test("Eval Networks", () => {
    let engine = newEngine()
    expect(engine.run("{a b c}")).toEqual({})
})

test("Eval Named Network", () => {
    let engine = newEngine()
    expect(engine.run("test {a b c} (read test)"))
        .toEqual({
            network: 
                [[{"identifier": "a"}, {"identifier": "b"}, {"identifier": "c"}]]})
})

// test("Test match", () => {
//     let engine = newEngine()
//     expect(engine.run("[match {$a b c} {a b c}]"))
//         .toEqual({name: "test", networks: [{name: "test", network: 
//             [[{"identifier": "a"}, {"identifier": "b"}, {"identifier": "c"}]]}]})
// })

// test("Print Network", () => {
//     let engine = newEngine()
//     let resNetwork = engine.run("{a b c}")
//     let resString = printNetwork(resNetwork)
//     expect(resString).toEqual("{ a b c, }")
// })

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
