open Utils
open JSONUtils
open CatalaRuntime
open Docx

let getKindName = (obj: JSON.t, _schema: JSON.t): string => {
  switch JSON.Classify.classify(obj) {
  | Object(items) =>
    switch items->Dict.get("default") {
    | Some(defaultVal) => JSON.Decode.string(defaultVal)->Option.getExn
    | None => Js.Exn.raiseError("invalid user inputs in [getKindName]")
    }
  | String(s) => s
  | _ => Js.Exn.raiseError("invalid user inputs in [getKindName]")
  }
}

let parseVarDefs = (~json, ~schema, ~uiSchema, ~keysToIgnore): array<var_def> => {
  let def = (name, value) => {
    pos: None,
    io: {io_input: NoInput, io_output: false},
    fun_calls: None,
    name,
    value,
  }

  // TODO: refactor with a context?
  let rec parse = (~currentSelectedEnumValue="", json: JSON.t, currentPath: list<string>): array<
    var_def,
  > => {
    switch JSON.Classify.classify(json) {
    | Object(items) =>
      switch (items->Dict.get("kind"), items->Dict.get("payload")) {
      | (Some(kindVal), None) =>
        let name = kindVal->getKindName(schema)
        [def(list{name}, Enum(list{}, (name, Unit)))]
      | (Some(kindVal), Some(payload)) if isEmptyJSON(payload) || isNullJSON(payload) =>
        let name = kindVal->getKindName(schema)
        [def(list{name}, Enum(list{}, (name, Unit)))]
      | (Some(kindVal), Some(payload)) => [
          def(
            list{kindVal->getKindName(schema)},
            parseValue(payload, currentPath, ~currentSelectedEnumValue),
          ),
        ]
      | (None, None) =>
        items
        ->Dict.toArray
        ->Array.map(((key, value)) => {
          let newPath = currentPath->List.concat(list{key})
          let name = newPath->findTitleInSchema(schema, ~currentSelectedEnumValue, ~keysToIgnore)
          // let tabLabel =
          //   uiSchema
          //   ->jsonGetPath(list{key, "ui:tabLabel"})
          //   ->Option.map(json =>
          //     json->JSON.Decode.string->getJsErr("'ui:tabLabel' should be a string in the uiSchema")
          // )

          let name = name->Option.getWithDefault(key)

          // FIXME: should correctly extract the name from the schema
          def(list{name}, value->parseValue(newPath, ~currentSelectedEnumValue))
        })
      | _ => failwith("invalid user inputs in [parse]")
      }
    | _ => failwith("invalid user inputs in [parse]")
    }
  }
  and parseValue = (
    ~currentSelectedEnumValue,
    json: JSON.t,
    currentPath: list<string>,
  ): LoggedValue.t => {
    switch JSON.Classify.classify(json) {
    | Object(items) =>
      switch (items->Dict.get("kind"), items->Dict.get("payload")) {
      | (Some(kindVal), None) => Enum(list{}, (kindVal->getKindName(schema), Unit))
      | (Some(kindVal), Some(payload)) if isEmptyJSON(payload) || isNullJSON(payload) =>
        Enum(list{}, (kindVal->getKindName(schema), Unit))
      | (Some(kindVal), Some(payload)) => {
          Console.log2("Struct parseValue: ", kindVal->getKindName(schema))
          let currentSelectedEnumValue = kindVal->getKindName(schema)
          Struct(
            list{currentSelectedEnumValue},
            parseFields(~currentSelectedEnumValue, payload, currentPath),
          )
        }
      | (None, None) => Struct(list{}, parseFields(json, currentPath, ~currentSelectedEnumValue))
      | _ => failwith("invalid user inputs in [parseValue]")
      }
    | Array(items) =>
      Array(items->Array.map(item => item->parseValue(currentPath, ~currentSelectedEnumValue)))
    | String(s) if s->isDate => Date(s)
    | String(s) => Duration(s)
    | Bool(b) => Bool(b)
    | Number(f) => Decimal(f)
    | Null => Unit
    }
  }
  and parseFields = (
    ~currentSelectedEnumValue: string,
    json: JSON.t,
    currentPath: list<string>,
  ): list<(string, LoggedValue.t)> => {
    switch JSON.Classify.classify(json) {
    | Object(items) =>
      switch (items->Dict.get("kind"), items->Dict.get("payload")) {
      | (None, None) =>
        items
        ->Dict.toArray
        ->Array.map(((key, value)) => {
          let newPath = currentPath->List.concat(list{key})
          let name = newPath->findTitleInSchema(schema, ~currentSelectedEnumValue, ~keysToIgnore)
          // let tabLabel =
          //   uiSchema
          //   ->jsonGetPath(list{key, "ui:tabLabel"})
          //   ->Option.map(json =>
          //     json->JSON.Decode.string->getJsErr("'ui:tabLabel' should be a string in the uiSchema")
          //   )

          let name = name->Option.getWithDefault(key)

          // FIXME: should correctly extract the name from the schema
          (
            name,
            value->parseValue(newPath, ~currentSelectedEnumValue),
            // tabLabel,
          )
        })
        ->List.fromArray
      | _ => failwith("invalid user inputs in [parseFields]")
      }
    | _ => failwith("invalid user inputs in [parseFields]")
    }
  }

  parse(json, list{})
}

let toTable = (inputs: array<var_def>) => {
  let headingText = "Informations renseignées par l'utilisateur·ice"
  let maxDepth = inputs->Utils.getMaxDepth
  let bgColor = #blue_france_925
  let contentRows = inputs->TableUtils.getTableRows(~bgColorRef=ref(bgColor), ~maxDepth)
  let headingParagraph = Paragraph.make'({
    spacing: {before: 80.0, after: 80.0},
    children: [
      TextRun.make'({
        bold: true,
        size: "10pt",
        text: headingText ++ " ",
      }),
    ],
  })

  TableUtils.getTable(~headingParagraph, ~contentRows, ~maxDepth, ~bgColor)->FileChild.fromTable
}
