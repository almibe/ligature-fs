import { expect, test } from 'vitest'
import { run as _run, runAndSelectElement } from './Library.fs.js'
import { element, Wander } from './model.js'

let wander: Wander = {
  run: (script) => {
    return _run(script)
  }
}

test('basic term', () => {
  expect(wander.run("test")).toStrictEqual({type:"Element",value:"test"})
})

test('basic element', () => {
  expect(wander.run("\"test\"")).toStrictEqual({type:"Element",value:"test"})
})

test('basic element', () => {
  expect(wander.run("element(a)")).toStrictEqual({type:"Element",value:"a"})
})

test('basic element with single link', () => {
  expect(wander.run("element(a b -> c)")).toStrictEqual(
    [{
      type:"ElementView",
      value: "a",
      links: {
        "b": [element("c")]
      }
    }])
})

// test('basic element with multiple links', () => {
//   expect(wander.run("element(a b -> c d -> e)")).toStrictEqual(
//     {
//       type:"Assertions",
//       assertions: new Set([
//         {
//           type: "Triple",
//           element: element("a"),
//           role: "b",
//           filler: element("c")
//         },
//         {
//           type: "Triple",
//           element: element("a"),
//           role: "d",
//           filler: element("e")
//         }])
//   })
// })

// test('basic element with link with seq', () => {
//   expect(wander.run("element(a b -> seq(c d))")).toStrictEqual(
//     {
//       type:"Assertions",
//       assertions: new Set([
//         {
//           type: "Triple",
//           element: element("a"),
//           role: "b",
//           filler: element("c")
//         },
//         {
//           type: "Triple",
//           element: element("a"),
//           role: "b",
//           filler: element("d")
//         }])
//   })
// })

// test('basic element with multiple links', () => {
//   expect(wander.run("element(a b -> c d -> seq(e f))")).toStrictEqual(
//     {
//       type:"Assertions",
//       assertions: new Set([
//         {
//           type: "Triple",
//           element: element("a"),
//           role: "b",
//           filler: element("c")
//         },
//         {
//           type: "Triple",
//           element: element("a"),
//           role: "d",
//           filler: element("e")
//         },
//         {
//           type: "Triple",
//           element: element("a"),
//           role: "d",
//           filler: element("f")
//         }])
//   })
// })

// test('call to runAndSelect', () => {
//   expect(runAndSelectElement("assertions(element(a b -> c))", "a")).toStrictEqual(
//     {
//       "type": "Element",
//       "value": "a",
//       "links": 
//         new Map([["b", [
//           { 
//             "type": "Element", 
//             "value": "c"
//           }
//         ]]])
//     })
// })
