open Docx

// let isPrimitive = (input: JSON.t): bool => {
//   switch JSON.Classify.classify(input) {
//   | Object(_) | Array(_) => false
//   | _ => true
//   }
// }
//
// let isEmpty = (inputs: JSON.t): bool => {
//   switch JSON.Classify.classify(inputs) {
//   | Object(fields) => Dict.toArray(fields) == []
//   | Array([]) => true
//   | _ => false
//   }
// }
//
// let isNull = (input: JSON.t): bool => {
//   input->JSON.Classify.classify == Null
// }

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

let isDate = (str: string): bool => {
  str->Date.fromString->Date.toString != "Invalid Date"
}

// let isEnum = (jsonSchema: JSON.t): bool => {
//   switch jsonSchema {
//   | Object(fields) =>
//     switch Dict.get(fields, "kind") {
//     | Some(_) => true
//     | None => false
//     }
//   | _ => false
//   }
// }
//
let getNextHeadingLevel = (lvl: HeadingLevel.t): HeadingLevel.t => {
  open HeadingLevel
  switch lvl {
  | lvl if lvl == h1 => h2
  | lvl if lvl == h2 => h3
  | lvl if lvl == h3 => h4
  | lvl if lvl == h4 => h5
  | lvl if lvl == h5 => h6
  | _ => h6
  }
}
