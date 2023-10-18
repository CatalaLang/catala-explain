open Utils
open JSONUtils
open CatalaRuntime
open Docx

let getCompleteEnumName = (~currentSelectedEnumValue="", jsonSchema, enumName, currentPath) => {
  switch JSON.Classify.classify(enumName) {
  | String(s) =>
    jsonSchema
    ->findEnumNameInSchema(~enumName=s, ~keys=currentPath, ~currentSelectedEnumValue)
    ->Option.getWithDefault(s)
  | _ => Js.Exn.raiseError("Enum name must be a string, got " ++ JSON.stringify(enumName))
  }
}

let parseVarDefs = (~json, ~schema, ~keysToIgnore): array<var_def> => {
  let def = (name, value) => {
    pos: None,
    io: {io_input: NoInput, io_output: false},
    fun_calls: None,
    name,
    value,
  }

  let rec parse = (~currentSelectedEnumValue="", ~path=list{}, json) => {
    switch JSON.Classify.classify(json) {
    | Object(items) =>
      switch (items->Dict.get("kind"), items->Dict.get("payload")) {
      | (Some(kindVal), None) =>
        let name = schema->getCompleteEnumName(kindVal, path)
        [def(list{name}, Enum(list{}, (name, Unit)))]
      | (Some(kindVal), Some(payload)) if isEmptyJSON(payload) || isNullJSON(payload) =>
        let name = schema->getCompleteEnumName(kindVal, path)
        [def(list{name}, Enum(list{}, (name, Unit)))]
      | (Some(kindVal), Some(payload)) => [
          def(
            list{schema->getCompleteEnumName(kindVal, path)},
            payload->parseValue(~path, ~currentSelectedEnumValue),
          ),
        ]
      | (None, None) =>
        items
        ->Dict.toArray
        ->Array.map(((key, value)) => {
          let newPath = path->List.concat(list{key})
          let name =
            schema->findTitleInSchema(~keys=newPath, ~keysToIgnore, ~currentSelectedEnumValue)
          let name = name->Option.getWithDefault(key)

          def(list{name}, value->parseValue(~path=newPath, ~currentSelectedEnumValue))
        })
      | _ => failwith("invalid user inputs in [parse]")
      }
    | _ => failwith("invalid user inputs in [parse]")
    }
  }
  and parseValue = (
    ~inArray=false,
    ~currentSelectedEnumValue,
    ~path: list<string>,
    json: JSON.t,
  ): LoggedValue.t => {
    switch JSON.Classify.classify(json) {
    | Object(items) =>
      switch (items->Dict.get("kind"), items->Dict.get("payload")) {
      | (Some(kindVal), None) =>
        Enum(list{}, (schema->getCompleteEnumName(kindVal, path, ~currentSelectedEnumValue), Unit))
      | (Some(kindVal), Some(payload)) if isEmptyJSON(payload) || isNullJSON(payload) => {
          let name = schema->getCompleteEnumName(kindVal, path, ~currentSelectedEnumValue)
          Enum(list{}, (name, Unit))
        }
      | (Some(kindVal), Some(payload)) => {
          let currentSelectedEnumValue = inArray
            ? kindVal->JSON.Decode.string->Option.getExn
            : schema->getCompleteEnumName(kindVal, path)
          Struct(
            list{currentSelectedEnumValue},
            payload->parseFields(path, ~currentSelectedEnumValue),
          )
        }
      | (None, None) => Struct(list{}, json->parseFields(path, ~currentSelectedEnumValue))
      | (None, Some(_)) =>
        Js.Exn.raiseError("Should not contain a 'payload' without a specified 'kind'")
      }
    | Array(items) =>
      Array(items->Array.map(parseValue(_, ~path, ~currentSelectedEnumValue, ~inArray=true)))
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
          let name =
            schema
            ->findTitleInSchema(~keys=newPath, ~currentSelectedEnumValue, ~keysToIgnore)
            ->Option.getWithDefault(key)

          (name, value->parseValue(~path=newPath, ~currentSelectedEnumValue))
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
