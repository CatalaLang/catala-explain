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
    Console.log2("1. Looking for", path->List.toArray)
    Console.log2("in", json)
    switch JSON.Classify.classify(json) {
    | Object(fields) if fields->Dict.get("anyOf") != None =>
      let hd = fields->Dict.get(head)
      // if head == "personnesACharge" || head == "nationalite" {
      // }
      hd->Option.flatMap(jsonGetPath(_, tail))
    | Object(fields) =>
      let hd = fields->Dict.get(head)
      Console.log2("hd", hd)
      hd->Option.flatMap(jsonGetPath(_, tail))
    | _ => None
    }
  }
}

// FIXME: still need to manage allOf, anyOf, etc...
// we need to find where there is a anyOf and then find the title in the schema
// by looking in each parent childs
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
        | Object(fields) if fields->Dict.get("anyOf") != None =>
          Console.log2("Found anyOf for", keys->List.toArray)
          None
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
