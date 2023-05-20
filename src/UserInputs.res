open Utils

type rec t =
  | Section({title: string, items: t})
  | Fields(array<t>)
  | Field({name: string, value: t})
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

let fromJSON = (~json: JSON.t, ~schema: JSON.t): t => {
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
            let name = newPath->findTitleInSchema(schema)

            // FIXME: should correctly extract the name from the schema
            Field({
              name: name == " " ? key : name,
              value: value->aux(newPath),
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
    | Array(items) =>
      // TODO: add section for each item
      Array(items->Array.map(item => item->aux(currentPath)))
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
  let litToStyledTextRun = (lit: t): paragraphChild => {
    switch lit {
    | LitBool(b) =>
      // TODO: manage the language
      TextRun.create'({
        text: b ? "vrai" : "faux",
        style: "BooleanLiteral",
      })
    | LitNumber(f) =>
      TextRun.create'({
        text: f->Float.toString,
        style: "NumberLiteral",
      })
    | LitDate(d) =>
      TextRun.create'({
        text: d
        ->Date.fromString
        ->Date.toLocaleDateStringWithLocaleAndOptions("fr-FR", {dateStyle: #long}),
        style: "DateLiteral",
      })
    | LitString(s) =>
      TextRun.create'({
        text: s,
        style: "StringLiteral",
      })
    | LitEnum(name) =>
      TextRun.create'({
        text: name,
        style: "EnumLiteral",
      })
    | LitNull => TextRun.create'({text: "Aucune entrée", style: "EmptyLiteral"})
    | _ => failwith("invalid user inputs in [litToStyledTextRun]")
    }
  }

  let toFileChild = (userInputs: t /* , _jsonSchema: JSON.t */): array<fileChild> => {
    let rec aux = (userInputs: t, level: HeadingLevel.t) => {
      switch userInputs {
      | Section({title, items}) =>
        [
          Paragraph.create'({
            children: [TextRun.create(title)],
            heading: level,
          }),
        ]->Array.concat(aux(items, getNextHeadingLevel(level)))
      | Array(inputs) =>
        // Need to have dot list ?
        inputs->Array.flatMap(input => aux(input, getNextHeadingLevel(level)))
      | Fields(items) =>
        items
        ->Array.sort((a, b) => {
          // Display section last
          switch (a, b) {
          | (Section(_), _) | (_, Section(_)) => -1
          | (Array(_), _) | (_, Array(_)) => -1
          | _ => 1
          }
        })
        ->Array.flatMap(input => aux(input, getNextHeadingLevel(level)))
      | Field({name, value}) if isLiteral(value) => [
          Paragraph.create'({
            children: [TextRun.create(`${name} : `), litToStyledTextRun(value)],
          }),
        ]
      | Field({value}) if isEmpty(value) => []
      | Field({name, value}) =>
        [
          Paragraph.create'({
            children: [TextRun.create(name)],
            heading: level,
          }),
        ]->Array.concat(aux(value, getNextHeadingLevel(level)))
      | _ => failwith("invalid user inputs in [userInputsToFileChild]")
      }
    }
    aux(userInputs, #Heading2)
  }
}
