import { expect, test } from 'vitest'
import { run as _run } from './Library.fs.js'
import { element, Wander } from './model.js'

let wander: Wander = {
  run: (script) => {
    return _run(script)
  }
}

test('basic term', () => {
  expect(wander.run("test")).toStrictEqual({type:"Term",value:"test"})
})

test('basic element', () => {
  expect(wander.run("\"test\"")).toStrictEqual({type:"Element",value:"test"})
})

test('basic object view', () => {
  expect(wander.run("element(a)")).toStrictEqual({type:"Element",value:"a"})
})

test('basic object view with single link', () => {
  expect(wander.run("element(a links(b = c))")).toStrictEqual(
    {
      type:"Element",
      value:"a",
      links: {"b": [element("c")]}
    })
})

test('basic object view with multiple links', () => {
  expect(wander.run("element(a links(b = c d = seq(e f)))")).toStrictEqual(
    {
      type:"Element",
      value:"a",
      links: {
        "b": [element("c")],
        "d": [element("e"), element("f")]
      }
    })
})

test('basic object view with multiple links', () => {
  expect(wander.run("element(a links(b = c d = seq(e f)))")).toStrictEqual(
    {
      type:"Element",
      value:"a",
      links: {
        "b": [element("c")],
        "d": [element("e"), element("f")]
      }
    })
})

test('nested object view', () => {
  expect(wander.run("element(a links(b = c d = seq(e element(f links(g = h)))))")).toStrictEqual(
    {
      type:"Element",
      value:"a",
      links: {
        "b": [element("c")],
        "d": [element("e"), 
            element("f", { "g": [element("h")] })]
      }
    })
})
