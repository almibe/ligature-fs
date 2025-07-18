export type Element = {
    type: "Element",
    value: string,
    namespace: string | null,
    langTag: any | null,
}

export type ObjectView = {
    type: "ObjectView"
    root: Element,
    links: Map<string, ObjectView[]>
}

export type Error = {
    type: "Error",
    message: string,
}
