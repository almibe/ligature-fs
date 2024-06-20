import { run } from "./lib/main.js"
import { glob } from "glob"
import fs from 'node:fs'
import { expect, test } from 'vitest'

const dir = process.env.LIGATURE_TEST_SUITE + "/wander/*.wander"
const wanderFiles = await glob(dir)

for (const file of wanderFiles) {
    test(`Testing ${file.replace("\\", "/").split("/").at(-1)}`, () => {
        const script = fs.readFileSync(file, 'utf8')
        const res = run(script)
        expect(res["tag"]).toBe(0)
    })
}
