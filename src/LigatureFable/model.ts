export type Term = string
export type Variable = string
export type Application = {
    type: "application",
    name: string,
    attributes: any,//Map<string, string>,
    body: any,
}

export type Node = {
    type: "node",
    name: string,
    attributes: any,//Map<string, string>,
    body: any,
}
