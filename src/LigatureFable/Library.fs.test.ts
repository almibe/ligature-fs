import { expect, test } from 'vitest'
import { run } from './Library.fs.js'

test('basic term', () => {
  expect(run("test")).toStrictEqual({type:"Term",value:"test"})
})

test('basic element', () => {
  expect(run("\"test\"")).toStrictEqual({type:"Element",value:"test"})
})

test('basic object view', () => {
  expect(run("element(a)")).toStrictEqual({type:"ObjectView",root:{type:"Element",value:"a"}})
})
