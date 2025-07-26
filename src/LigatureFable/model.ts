export type Element = {
    type: "Element",
    value: string,
    namespace?: string,
    langTag?: string,
    links?: Record<string, Element[]>
}

export type Triple = {
    type: "Triple",
    element: Element,
    role: string,
    filler: Element
}

export type Instance = {
    type: "Instance",
    element: Element,
    concept: string
}

export type Assertion = Triple | Instance

export type Assertions = {
    type: "Assertions",
    assertions: Set<Assertion>
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

export interface Wander {
    run(script: string, fns?: Map<string, (arg: Assertions) => void>): Error | Assertions | Element
}
