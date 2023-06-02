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

let rec jsonGetPath = (json: JSON.t, path: list<string>): option<JSON.t> => {
  switch path {
  | list{} => Some(json)
  | list{head, ...tail} =>
    switch JSON.Classify.classify(json) {
    | Object(fields) => fields->Dict.get(head)->Option.flatMap(jsonGetPath(_, tail))
    | _ => None
    }
  }
}

// FIXME: still need to manage allOf, anyOf, etc...
let findTitleInSchema = (keys: list<string>, jsonSchema: JSON.t): option<string> => {
  let getDefinition = (ref: string): option<JSON.t> => {
    switch ref->String.split("/") {
    | ["#", "definitions", key] => jsonSchema->jsonGetPath(list{"definitions", key})
    | _ => None
    }
  }
  let rec aux = (keys: list<string>, json: JSON.t): option<string> => {
    switch keys {
    | list{} => None
    | list{head} =>
      jsonGetPath(json, list{"properties", head})->Option.flatMap(fields => {
        switch JSON.Classify.classify(fields) {
        | Object(fields) => fields->Dict.get("title")->Option.flatMap(JSON.Decode.string)
        | _ => None
        }
      })
    | list{head, ...tail} =>
      jsonGetPath(json, list{"properties", head})->Option.flatMap(fields => {
        switch JSON.Classify.classify(fields) {
        | Object(fields) =>
          Dict.get(fields, "$ref")
          ->Option.flatMap(JSON.Decode.string)
          ->Option.flatMap(getDefinition)
          ->Option.flatMap(aux(tail, _))
        | _ => None
        }
      })
    }
  }
  aux(keys, jsonSchema)
}
