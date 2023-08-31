open Utils
open JSONUtils

type rec t =
  | Section({title: string, items: t})
  | Fields(array<t>)
  | Field({name: string, value: t, tabLabel: option<string>})
  | Array(array<t>)
  | LitEnum(string)
  | LitDate(string)
  | LitString(string)
  | LitNumber(float)
  | LitBool(bool)
  | LitNull

let isLiteral = (t: t): bool => {
  switch t {
  | LitEnum(_) | LitDate(_) | LitString(_) | LitNumber(_) | LitBool(_) | LitNull => true
  | _ => false
  }
}

let compare = (v: t, v': t): int => {
  let order = (v: t): int => {
    switch v {
    | Section(_) => 3
    | Fields(_) => 2
    | Array(_) => 1
    | _ => 0
    }
  }

  order(v) - order(v')
}

let rec isEmpty = (t: t): bool => {
  switch t {
  | Section({items}) => isEmpty(items)
  | Field({value}) => isEmpty(value)
  | Fields(items) | Array(items) => items->Array.length == 0
  | _ => false
  }
}

let getKindName = (obj: JSON.t, _schema: JSON.t): string => {
  switch JSON.Classify.classify(obj) {
  | Object(items) =>
    switch items->Dict.get("default") {
    | Some(defaultVal) => JSON.Decode.string(defaultVal)->Option.getExn
    | None => failwith("invalid user inputs in [getKindName]")
    }
  | String(s) => s
  | _ => failwith("invalid user inputs in [getKindName]")
  }
}

let fromJSON = (~json: JSON.t, ~schema: JSON.t, ~uiSchema: JSON.t): t => {
  let rec aux = (json: JSON.t, currentPath: list<string>): t => {
    switch JSON.Classify.classify(json) {
    | Object(items) =>
      switch (items->Dict.get("kind"), items->Dict.get("payload")) {
      | (None, None) =>
        Fields(
          items
          ->Dict.toArray
          ->Array.map(((key, value)) => {
            let newPath = currentPath->List.concat(list{key})
            if key == "nationalite" {
              Console.log2("=====> DEBUG newPath:", newPath->List.toArray)
              // Console.log2("=====> DEBUG name:", name)
            }
            let name = newPath->findTitleInSchema(schema)
            let tabLabel =
              uiSchema
              ->jsonGetPath(list{key, "ui:tabLabel"})
              ->Option.map(json =>
                json
                ->JSON.Decode.string
                ->getJsErr("'ui:tabLabel' should be a string in the uiSchema")
              )

            let name = name->Option.getWithDefault(key)

            // FIXME: should correctly extract the name from the schema
            Field({
              name,
              value: value->aux(newPath),
              tabLabel,
            })
          }),
        )
      | (Some(kindVal), None) => LitEnum(kindVal->getKindName(schema))
      | (Some(kindVal), Some(payload)) if isEmptyJSON(payload) || isNullJSON(payload) =>
        LitEnum(kindVal->getKindName(schema))
      | (Some(kindVal), Some(payload)) =>
        Section({
          title: kindVal->getKindName(schema),
          items: payload->aux(currentPath),
        })
      | _ => failwith("invalid user inputs in [fromJSON]")
      }
    | Array(items) => Array(items->Array.map(item => item->aux(currentPath)))
    | String(s) =>
      if isDate(s) {
        LitDate(s)
      } else {
        LitString(s)
      }
    | Bool(b) => LitBool(b)
    | Number(f) => LitNumber(f)
    | Null => LitNull
    }
  }
  aux(json, list{})
}

module Docx = {
  open Docx

  // TODO: manage the language
  let litToStyledTextRun = (lit: t): ParagraphChild.t => {
    switch lit {
    | LitBool(b) =>
      TextRun.make'({
        text: b ? "oui" : "non",
        style: "BooleanLiteral",
      })
    | LitNumber(f) =>
      TextRun.make'({
        text: f->Float.toString,
        style: "NumberLiteral",
      })
    | LitDate(d) =>
      TextRun.make'({
        text: d
        ->Date.fromString
        ->Date.toLocaleDateStringWithLocaleAndOptions("fr-FR", {dateStyle: #long}),
        style: "DateLiteral",
      })
    | LitString(s) =>
      TextRun.make'({
        text: s,
        style: "StringLiteral",
      })
    | LitEnum(name) =>
      TextRun.make'({
        text: name,
        style: "EnumLiteral",
      })
    | LitNull => TextRun.make'({text: "Aucune entrée", style: "EmptyLiteral"})
    | _ => failwith("invalid user inputs in [litToStyledTextRun]")
    }
  }

  let toFileChild = (userInputs: t): array<FileChild.t> => {
    let rec aux = (~elemId=?, ~level: HeadingLevel.t, ~prevInput: t, input: t) => {
      open FileChild
      switch input {
      | Section({title, items}) =>
        [
          p'({
            children: [
              TextRun.make(
                switch (prevInput, elemId) {
                | (Array(_), Some(id)) => `${title} n°${id->Int.toString}`
                | _ => title
                },
              ),
            ],
            heading: level,
          }),
        ]->Array.concat(items->aux(~prevInput=input, ~level=getNextHeadingLevel(level)))
      | Array(inputs) =>
        let id = ref(0)
        inputs->Array.flatMap(arrayInput => {
          id := id.contents + 1
          arrayInput->aux(~elemId=id.contents, ~prevInput=input, ~level=getNextHeadingLevel(level))
        })
      | Fields(items) =>
        items
        ->Array.sort((a, b) => {
          switch (a, b) {
          | (Field({value: v}), Field({value: v'})) => compare(v, v')
          | _ => Js.Exn.raiseError("[Fields] expects to only contains [Field] values")
          }
        })
        ->Array.flatMap(i => i->aux(~prevInput=input, ~level=getNextHeadingLevel(level)))
      | Field({name, value}) if value->isLiteral => [
          p'({
            children: [TextRun.make(`${name} : `), litToStyledTextRun(value)],
          }),
        ]
      | Field({value}) if value->isEmpty => []
      | Field({name, value}) =>
        [
          p'({
            children: [TextRun.make(name)],
            heading: level,
          }),
        ]->Array.concat(value->aux(~prevInput=input, ~level=getNextHeadingLevel(level)))
      | _ => failwith("invalid user inputs in [userInputsToFileChild]")
      }
    }
    userInputs->aux(~prevInput=userInputs, ~level=#Heading2)
  }
}
