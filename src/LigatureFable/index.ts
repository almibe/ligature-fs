import { run as _run } from './Library.fs.js'

export type Namespace = string
export type LangTag = string
export type Elem = {
    type: "element",
    value: string,
    namespace: Namespace,
    langTag: LangTag,
}

export type Role = string

export type AtomicConcept = {
    type: "atomicConcept",
    name: string
}

export type Not = {
    type: "not",
    concept: ConceptExpr
}

export type Func = {
    type: "func",
    role: Role
}

export type Exists = {
    type: "exists",
    role: Role,
    concept: ConceptExpr | null
}

export type All = {
    type: "all",
    role: Role,
    concept: ConceptExpr | null
}

export type And = {
    type: "and",
    concepts: ConceptExpr[]
}

export type Or = {
    type: "or",
    concepts: ConceptExpr[]
}

export type Top = {
    type: "top"
}

export type Bottom = {
    type: "bottom"
}

export type ConceptExpr = AtomicConcept | Not | Func | Exists | All | And | Or | Top | Bottom

export type Same = {
    type: "same",
    left: Elem,
    right: Elem
}

export type Different = {
    type: "different",
    left: Elem,
    right: Elem
}

export type Instance = {
    type: "instance",
    elem: Elem,
    concept: ConceptExpr
}

export type Triple = {
    type: "triple",
    elem: Elem,
    role: Role,
    filler: Elem
}

export type Assertions = Same | Different | Instance | Triple

export type Implies = {
    type: "implies",
    left: ConceptExpr,
    right: ConceptExpr
}

export type Equivalent = {
    type: "equivalent",
    left: ConceptExpr,
    right: ConceptExpr
}

export type Definition = Implies | Equivalent

export function run(script: string) {
    return _run(script)
}
