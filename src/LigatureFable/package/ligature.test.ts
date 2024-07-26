import { run, coreBindings, WanderFunction, bindFunction } from "./lib/ligature.ts"
import { glob } from "glob"
import fs from 'node:fs'
import { expect, test } from 'vitest'

const dir = process.env.LIGATURE_TEST_SUITE + "/wander/*.wander"
const wanderFiles = await glob(dir)

// for (const file of wanderFiles) {
//     test(`Testing ${file.replace("\\", "/").split("/").at(-1)}`, () => {
//         const script = fs.readFileSync(file, 'utf8')
//         const res = run(script)
//         expect(res.error).toEqual(undefined)
//         expect(res.length).toEqual(0)
//     })
// }

test("Basic values", () => {
//    expect(run("{}")).toEqual([])
    expect(run("24601"), [], []).toEqual(24601n)
    // expect(run('"test"')).toEqual("test")
    // expect(run("`test`")).toEqual({"identifier": "test"})
    // expect(run("$test")).toEqual({"slot": "test"})
})

// test("Eval Networks", () => {
//     expect(run("{`a` `b` `c`}")).toEqual([
//         [{"identifier": "a"}, {"identifier": "b"}, {"identifier": "c"}]
//     ])
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
