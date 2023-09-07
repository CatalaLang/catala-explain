let isEmptyJSON = (inputs: JSON.t): bool => {
  switch JSON.Classify.classify(inputs) {
  | Object(fields) => Dict.toArray(fields) == []
  | Array([]) => true
  | _ => false
  }
}

let isNullJSON = (input: JSON.t): bool => {
  input->JSON.Classify.classify == Null
}

type allOfCondition = {
  "if": {"properties": {"kind": {"const": string}}},
  "then": {"properties": {"payload": {"title": string, "$ref": string}}},
}

@scope("JSON") @val external parseAllOfCondition: string => allOfCondition = "parse"

/**

Structure of the expected JSON:
 "allOf": [
        {
          "if": {
            "properties": {
              "kind": {
                "const": "Locataire" <-- Should match the enumName
              }
            }
          },
          "then": {
            "properties": {
              "payload": {
                "title": " ",
                "$ref": "#/definitions/location" <-- WANTED
              }
            }
          }
        },
		...
	]
* 
*/
let getEnumPayloadRefFromAllOf = (enumName: string, json) => {
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
    switch ref {
    | Some(r) => r
    | None => Js.Exn.raiseError(`Can't find corresponding $ref for enum: ${enumName}`)
    }
  | _ => Js.Exn.raiseError("AllOf element should be an array")
  }
}

// // TODO: Add a generic function to get field of a JSON object following $refs
let findTitleInSchema = (
  ~currentSelectedEnumValue: string,
  ~keysToIgnore: array<string>,
  keys: list<string>,
  jsonSchema: JSON.t,
): option<string> => {
  let rec aux = (~path, ~json): option<string> => {
    switch path {
    | list{} => None
    | list{head} =>
      jsonGetPath(~path=list{"properties", head}, ~json)->Option.flatMap(fields => {
        switch JSON.Classify.classify(fields) {
        | Object(fields) => fields->Dict.get("title")->Option.flatMap(JSON.Decode.string)
        | _ => None
        }
      })
    | list{head, ...tail} =>
      jsonGetPath(~path=list{"properties", head}, ~json)->Option.flatMap(fields => {
        switch JSON.Classify.classify(fields) {
        | Object(fields) =>
          let obj =
            fields->Dict.get("type") == Some(JSON.Encode.string("array"))
              ? // If array get the $ref in items
                fields->Dict.get("items")->Option.getExn->JSON.Decode.object->Option.getExn
              : fields

          switch obj->Dict.get("$ref") {
          | Some(ref) =>
            ref
            ->JSON.Decode.string
            ->Option.flatMap(getDefinition(_))
            ->Option.flatMap(aux(~path=tail, ~json=_))
          | None =>
            Console.error2("Can't find the '$ref' field in", fields)
            Js.Exn.raiseError("See previous message :)")
          }
        | _ => None
        }
      })
    }
  }
  and jsonGetPath = (~json, ~path) => {
    switch path {
    | list{} => Some(json)
    | list{head, ...tail} if !(keysToIgnore->Array.includes(head)) =>
      switch JSON.Classify.classify(json) {
      | Object(fields) if fields->Dict.get("allOf") != None => {
          let enumPayloadRef = getEnumPayloadRefFromAllOf(
            currentSelectedEnumValue,
            fields->Dict.get("allOf")->Option.getUnsafe,
          )
          switch getDefinition(enumPayloadRef) {
          | Some(json) => jsonGetPath(~json, ~path=list{"properties", ...tail})
          | None =>
            Js.Exn.raiseError(`Can't find the definition corresponding to '${enumPayloadRef}'`)
          }
        }
      | Object(fields) =>
        switch fields->Dict.get(head) {
        | Some(json) => jsonGetPath(~json, ~path=tail)
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

  and getDefinition = ref => {
    switch ref->String.split("/") {
    | ["#", "definitions", key] => jsonGetPath(~json=jsonSchema, ~path=list{"definitions", key})
    | _ => None
    }
  }
  aux(~path=keys, ~json=jsonSchema)
}
