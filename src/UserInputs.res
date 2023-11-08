open Utils
open JSONUtils
open CatalaRuntime
open Docx

let getCompleteEnumName = (~parentEnums, jsonSchema, enumName, currentPath) => {
  switch JSON.Classify.classify(enumName) {
  | String(s) =>
    jsonSchema
    ->findEnumNameInSchema(~enumName=s, ~keys=currentPath, ~parentEnums)
    ->Option.getWithDefault(s)
  | _ => Js.Exn.raiseError("Enum name must be a string, got " ++ JSON.stringify(enumName))
  }
}

let parseVarDefs = (~json, ~schema): array<var_def> => {
  let def = (name, value) => {
    pos: None,
    io: {io_input: NoInput, io_output: false},
    fun_calls: None,
    name,
    value,
  }

  let rec parse = (~parentEnums: array<string>=[], ~path: list<string>=list{}, json) => {
    switch JSON.Classify.classify(json) {
    | Object(items) =>
      switch (items->Dict.get("kind"), items->Dict.get("payload")) {
      | (Some(kindVal), None) =>
        let name = schema->getCompleteEnumName(kindVal, path, ~parentEnums)
        [def(list{name}, Enum(list{}, (name, Unit)))]
      | (Some(kindVal), Some(payload)) if isEmptyJSON(payload) || isNullJSON(payload) =>
        let name = schema->getCompleteEnumName(kindVal, path, ~parentEnums)
        [def(list{name}, Enum(list{}, (name, Unit)))]
      | (Some(kindVal), Some(payload)) => [
          def(
            list{schema->getCompleteEnumName(kindVal, path, ~parentEnums)},
            payload->parseValue(~path, ~parentEnums),
          ),
        ]
      | (None, None) =>
        items
        ->Dict.toArray
        ->Array.map(((key, value)) => {
          let newPath = path->List.concat(list{key})
          let name = schema->findTitleInSchema(~keys=newPath, ~parentEnums)
          let name = name->Option.getWithDefault(key)

          def(list{name}, value->parseValue(~path=newPath, ~parentEnums))
        })
      | _ => failwith("invalid user inputs in [parse]")
      }
    | _ => failwith("invalid user inputs in [parse]")
    }
  }
  and parseValue = (~parentEnums, ~path, json): LoggedValue.t => {
    switch JSON.Classify.classify(json) {
    | Object(items) =>
      switch (items->Dict.get("kind"), items->Dict.get("payload")) {
      | (Some(kindVal), None) =>
        Enum(list{}, (schema->getCompleteEnumName(kindVal, path, ~parentEnums), Unit))
      | (Some(kindVal), Some(payload)) if isEmptyJSON(payload) || isNullJSON(payload) => {
          let name = schema->getCompleteEnumName(kindVal, path, ~parentEnums)
          Enum(list{}, (name, Unit))
        }
      | (Some(kindVal), Some(payload)) => {
          let newSelectedEnum = kindVal->JSON.Decode.string->Option.getExn
          parentEnums->Array.push(newSelectedEnum)
          Struct(list{newSelectedEnum}, payload->parseFields(path, ~parentEnums))
        }
      | (None, None) => Struct(list{}, json->parseFields(path, ~parentEnums))
      | (None, Some(_)) =>
        Js.Exn.raiseError("Should not contain a 'payload' without a specified 'kind'")
      }
    | Array(items) => Array(items->Array.map(parseValue(_, ~path, ~parentEnums)))
    | String(s) if s->isDate => Date(s)
    | String(s) => Duration(s)
    | Bool(b) => Bool(b)
    | Number(f) => Decimal(f)
    | Null => Unit
    }
  }
  and parseFields = (~parentEnums, json, currentPath): list<(string, LoggedValue.t)> => {
    switch JSON.Classify.classify(json) {
    | Object(items) =>
      switch (items->Dict.get("kind"), items->Dict.get("payload")) {
      | (None, None) =>
        items
        ->Dict.toArray
        ->Array.map(((key, value)) => {
          let newPath = currentPath->List.concat(list{key})
          let name =
            schema->findTitleInSchema(~keys=newPath, ~parentEnums)->Option.getWithDefault(key)

          (name, value->parseValue(~path=newPath, ~parentEnums))
        })
        ->List.fromArray
      | _ => Js.Exn.raiseError("Should not contain 'kind' or 'payload'")
      }
    | _ => Js.Exn.raiseError("Expected an object, got " ++ JSON.stringify(json))
    }
  }

  parse(json)
}

let toTable = (inputs: array<var_def>) => {
  let headingText = "Informations renseignées par vous dans le formulaire"
  let maxDepth = inputs->Utils.getMaxDepth
  let bgColor = #blue
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
