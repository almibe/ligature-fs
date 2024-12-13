import { runScript, readValue as read } from "../../output/LigatureFable.js"
import { Record, Set, List } from "immutable"
import type { RecordOf } from 'immutable'


type ElementProps = {type: "element", name: string}
const Element: Record.Factory<ElementProps> = Record({type: "element", name: ""})
type Element = RecordOf<ElementProps>

type VariableProps = { type: "variable", name: string }
const Variable = Record({ type: "variable", name: "" })
type Variable = RecordOf<VariableProps>

type LiteralProps = { type: "literal", value: string }
const Literal = Record({ type: "literal", value: "" })
type Literal = RecordOf<LiteralProps>

type Quote = List<Value>

type Value = Element | Variable | Literal | Quote

type ExtendsProps = { type: "extends", element: Element, concept: Element }
const Extends: Record.Factory<ExtendsProps> = Record({ type: "extends", element: Element(), concept: Element() })
type Extends = RecordOf<ExtendsProps>

type NotExtendsProps = { type: "notextends", element: Element, concept: Element }
const NotExtends: Record.Factory<NotExtendsProps> = Record({ type: "notextends", element: Element(), concept: Element() })
type NotExtends = RecordOf<NotExtendsProps>

type AttributeProps = { type: "attribute", element: Element, attribute: Element, value: Value }
const Attribute: Record.Factory<AttributeProps> = Record({ type: "attribute", element: Element(), attribute: Element(), value: Element() })
type Attribute = RecordOf<AttributeProps>

type Entry = Attribute | NotExtends | Extends

type Network = Set<Entry>

export function run(script: string): Map<string, any> {
    return runScript(script)
}

export function readValue(input: string): any {
    return read(input)
}
