import { run, coreBindings } from "./lib/ligature.ts"
import { glob } from "glob"
import fs from 'node:fs'
import { expect, test } from 'vitest'
import { bindFunction } from "../Ligature.fs.js"
import { HostFunction } from "../../Ligature/wander/Model.fs.js"
import { singleton, head } from "../fable_modules/fable-library-js.4.19.2/List"
import { WanderType } from "../../Ligature/wander/Model.fs.js"
import { FSharpResult$2 } from "../fable_modules/fable-library-js.4.19.2/Result"
import { WanderType, WanderValue } from "../../Ligature/wander/Model.fs.js"
import { fromInt64 } from "../fable_modules/fable-library-js.4.19.2/BigInt"

const dir = process.env.LIGATURE_TEST_SUITE + "/wander/*.wander"
const wanderFiles = await glob(dir)

for (const file of wanderFiles) {
    test(`Testing ${file.replace("\\", "/").split("/").at(-1)}`, () => {
        const script = fs.readFileSync(file, 'utf8')
        const res = run(script)
        expect(res.error).toEqual(undefined)
        expect(res.length).toEqual(0)
    })
}

test("Basic values", () => {
    expect(run("{}")).toEqual([])
    expect(run("24601")).toEqual(24601n)
    expect(run('"test"')).toEqual("test")
    expect(run("`test`")).toEqual({"identifier": "test"})
    expect(run("$test")).toEqual({"slot": "test"})
})

test("Eval Networks", () => {
    expect(run("{`a` `b` `c`}")).toEqual([
        [{"identifier": "a"}, {"identifier": "b"}, {"identifier": "c"}]
    ])
})

test("Add HostFunction from JS", () => {

    const addFunction = new HostFunction(
        "Test",
        "test", 
        singleton([]),
        new WanderType(1, []),
        "Add two numbers.", 
        (args, _arg) => new FSharpResult$2(0, [new WanderValue(1, [
            "test"
        ])]));

    const newBindings = bindFunction(addFunction, coreBindings)
    expect(run("test ()", newBindings)).toEqual("test")
})
