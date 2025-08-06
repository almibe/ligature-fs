import { runWithFns } from "./Library.fs.js"

export type Element = {
    type: "Element",
    value: string,
    namespace?: string,
    langTag?: string,
}

export type ElementView = {
    type: "ElementView",
    value: string,
    namespace?: string,
    langTag?: string,
    links: Record<string, Element[]>
}

export type Error = {
    type: "Error",
    message: string,
}

export function element(value: string): Element {
    return {
        type: "Element",
        value: value,
    }
}

export function run(script: string, fns?: Map<string, (arg: ElementView[]) => void>) {
    runWithFns(script, fns)
}
