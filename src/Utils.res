let isPrimitive = (input: JSON.t): bool => {
  switch JSON.Classify.classify(input) {
  | Object(_) | Array(_) => false
  | _ => true
  }
}

let isEmpty = (inputs: JSON.t): bool => {
  switch JSON.Classify.classify(inputs) {
  | Object(fields) => Dict.toArray(fields) == []
  | Array([]) => true
  | _ => false
  }
}

let isNull = (input: JSON.t): bool => {
  input->JSON.Classify.classify == Null
}

let rec jsonGetPath = (json: JSON.t, path: list<string>): option<JSON.t> => {
  switch path {
  | list{} => Some(json)
  | list{head, ...tail} =>
    switch JSON.Classify.classify(json) {
    | Object(fields) => Dict.get(fields, head)->Option.flatMap(field => jsonGetPath(field, tail))
    | _ => None
    }
  }
}

// TODO: manage enums and refs in schema
let findTitleInSchema = (key: string, jsonSchema: JSON.t): string => {
  jsonGetPath(jsonSchema, list{"properties", key, "title"})
  ->Option.flatMap(JSON.Decode.string)
  ->Option.getWithDefault(key)
}
