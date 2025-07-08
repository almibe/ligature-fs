import { expect, test } from 'vitest'
import { run } from './Library.fs.js'

test('basic term', () => {
  expect(run("test")).toStrictEqual({type:"term",value:"test"})
})

test('basic term', () => {
  expect(run("\"test\"")).toStrictEqual({type:"term",value:"test"})
})
