/*
 * Contains utility functions to manipulate JSON schemas
 */

let isEmptyJSON = json => {
  switch JSON.Classify.classify(json) {
  | Object(fields) => Dict.toArray(fields) == []
  | Array([]) => true
  | _ => false
  }
}

let isNullJSON = json => {
  json->JSON.Classify.classify == Null
}

let getOptionFlatMap = (json, key) => {
  json->Option.flatMap(json =>
    json->JSON.Decode.object->Option.flatMap(fields => fields->Dict.get(key))
  )
}

type allOfCondition = {
  "if": {"properties": {"kind": {"const": string}}},
  "then": {"properties": {"payload": {"title": string, "$ref": string}}},
}

@scope("JSON") @val external parseAllOfCondition: string => allOfCondition = "parse"

let getEnumPayloadRefFromAllOf = (json, enumName) => {
  switch JSON.Classify.classify(json) {
  | Array(items) =>
    let ref = items->Array.findMap(item => {
      let allOfCondition = item->JSON.stringify->parseAllOfCondition
      if allOfCondition["if"]["properties"]["kind"]["const"] == enumName {
        Some(allOfCondition["then"]["properties"]["payload"]["$ref"])
      } else {
        None
      }
    })
    ref
  | _ => Js.Exn.raiseError("AllOf element should be an array")
  }
}

type enumDefObject = {
  "properties": {
    "kind": {"anyOf": array<{"title": string, "type": string, "enum": array<string>}>},
  },
}

@scope("JSON") @val external parseEnumDefObject: string => enumDefObject = "parse"

let getEnumTitleFromEnumDefObject = (json, enumName) => {
  let enumDefObject = json->JSON.stringify->parseEnumDefObject
  let enumDef = enumDefObject["properties"]["kind"]["anyOf"]
  enumDef->Array.findMap(item => {
    if item["enum"]->Array.includes(enumName) {
      Some(item["title"])
    } else {
      None
    }
  })
}

let getDefinition = (json, ref) => {
  switch ref->String.split("/") {
  | ["#", "definitions", key] => json->Some->getOptionFlatMap("definitions")->getOptionFlatMap(key)
  | _ => None
  }
}

let getReference = (json, fields) => {
  let obj =
    fields->Dict.get("type") == Some(JSON.Encode.string("array"))
      ? // If array get the $ref in items
        fields->Dict.get("items")->Option.getExn->JSON.Decode.object->Option.getExn
      : fields

  switch obj->Dict.get("$ref") {
  | Some(ref) => ref->JSON.Decode.string->Option.flatMap(json->getDefinition(_))
  | None => Js.Exn.raiseError("See previous message :)")
  }
}

let getAtPath = (jsonSchema, ~parentEnums, ~keysToIgnore, ~path) => {
  let rec getInProperties = (json, path): option<JSON.t> => {
    switch path {
    | list{} => None
    | list{head} => json->getAt(list{"properties", head})
    | list{head, ...tail} =>
      json
      ->getAt(list{"properties", head})
      ->Option.flatMap(fields => {
        switch JSON.Classify.classify(fields) {
        | Object(fields) =>
          jsonSchema->getReference(fields)->Option.flatMap(getInProperties(_, tail))
        | _ => None
        }
      })
    }
  }
  and getAt = (json, path) => {
    switch path {
    | list{} => Some(json)
    | list{head, ...tail} if !(keysToIgnore->Array.includes(head)) =>
      switch JSON.Classify.classify(json) {
      | Object(fields) if fields->Dict.get("allOf") != None => {
          let enumPayloadRef = parentEnums->Array.findMap(enumValue => {
            fields->Dict.get("allOf")->Option.getUnsafe->getEnumPayloadRefFromAllOf(enumValue)
          })
          switch enumPayloadRef {
          | None =>
            Js.Exn.raiseError(
              `Can't find the enumPayloadRef for one of: ${parentEnums->Array.toString}`,
            )
          | Some(ref) =>
            switch jsonSchema->getDefinition(ref) {
            | Some(json) => json->getAt(list{"properties", ...tail})
            | None => Js.Exn.raiseError(`Can't find the definition corresponding to '${ref}'`)
            }
          }
        }
      | Object(fields) =>
        switch fields->Dict.get(head) {
        | Some(json) => json->getAt(tail)
        | None =>
          Js.Exn.raiseError(
            `The key '${head}' isn't included in: ${fields->JSON.stringifyAny->Option.getUnsafe}`,
          )
        }
      | _ => None
      }
    | _ => None
    }
  }

  jsonSchema->getInProperties(path)
}

let findTitleInSchema = (
  jsonSchema: JSON.t,
  ~parentEnums: array<string>,
  ~keys: list<string>,
): option<string> => {
  jsonSchema
  ->getAtPath(~path=keys, ~parentEnums, ~keysToIgnore=Context.keysToIgnore.contents)
  ->Option.flatMap(fields => {
    switch JSON.Classify.classify(fields) {
    | Object(fields) => fields->Dict.get("title")->Option.flatMap(JSON.Decode.string)
    | _ => None
    }
  })
}

let findEnumNameInSchema = (
  jsonSchema: JSON.t,
  ~parentEnums: array<string>,
  ~enumName: string,
  ~keys: list<string>,
): option<string> => {
  Array.push(parentEnums, enumName)
  switch jsonSchema->getAtPath(~path=keys, ~parentEnums, ~keysToIgnore=[]) {
  | Some(fields) =>
    switch JSON.Classify.classify(fields) {
    | Object(fields) =>
      jsonSchema->getReference(fields)->Option.flatMap(getEnumTitleFromEnumDefObject(_, enumName))
    | _ => Js.Exn.raiseError(`Should be an object: ${fields->JSON.stringifyAny->Option.getUnsafe}`)
    }
  | None => Js.Exn.raiseError(`Can't find: ${keys->List.toArray->Array.toString}`)
  }
}
