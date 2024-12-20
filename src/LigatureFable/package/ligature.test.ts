import { createCommand } from "../output/LigatureFable";
import { run, runResult, } from "./lib/ligature"
// import { glob } from "glob"
// import fs from 'node:fs'
import { expect, test } from 'vitest'

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

// test("Eval Named Network With Role", () => {
//     expect(run("let test {a b c}"))
//         .toEqual(
//                 {"test": {nodes: [{key: "a"}, {key: "c"}], edges: [{source: "a", target: "c", key:"b"}]}})
// })

// test("test reading empty network", () => {
//     expect(readValue("{}"))
//         .toEqual(
//                 {entries: []})
// })

// test("test reading simple network", () => {
//     expect(readValue("{a b c}"))
//         .toEqual(
//                 {entries: [
//                     { first: "a", second: "b", third: "c" }
//                 ]})
// })

test("test adding commands from js", () => {
    let x =0;
    let command = createCommand({
        name: "test",
        "doc": "doc",
        "action": (args) => {
            expect(args.length).toEqual(1)
            x=1 
        }
    })
    runResult("test {a b c}, match {} {}", [command])
    expect(x).toEqual(1)
})
