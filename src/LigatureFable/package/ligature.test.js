import { run } from "./lib/ligature.ts"
import { glob } from "glob"
import fs from 'node:fs'
import { expect, test } from 'vitest'

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
