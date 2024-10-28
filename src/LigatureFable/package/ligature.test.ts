import { newEngine, run } from "./lib/ligature.js"
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
    expect(run("let test {a b c}, read test"))
        .toEqual(
                {"test": [{type: "role", first: {symbol: "a"}, second: { symbol: "c"}, role: {symbol: "b"}}]})
})

test("Eval Named Network With Extension", () => {
    expect(run("let test {betty : Cat}, read test"))
        .toEqual( 
                {"test": [{type: "extension", element: {symbol: "betty"}, concept: { symbol: "Cat"}}]})
})

test("Empty Network", () => {
    let engine = newEngine()
    expect(engine.run("let test {}")).toEqual({})
})

test("Eval Networks", () => {
    let engine = newEngine()
    expect(engine.run("let test {a b c}")).toEqual({})
})

test("Eval Named Network", () => {
    let engine = newEngine()
    expect(engine.run("let test {a b c}, read test"))
        .toEqual({
            network: 
                [{type: "role", first: {symbol: "a"}, second: { symbol: "c"}, role: {symbol: "b"}}]})
})
