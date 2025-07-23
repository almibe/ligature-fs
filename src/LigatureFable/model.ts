export type Links = Record<string, Element[]>

export type Element = {
    type: "Element",
    value: string,
    namespace?: string,
    langTag?: string,
    links?: Links
}

export type Error = {
    type: "Error",
    message: string,
}

export function element(value: string, links?: Links): Element {
    if (links) {
        return {
            type: "Element",
            value: value,
            links: links
        }
    } else {
        return {
            type: "Element",
            value: value,
        }
    }
}

export interface Wander {
    run(script: string): Error | (Element[])
}
