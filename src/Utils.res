open Docx

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
    | Object(fields) => Dict.get(fields, head)->Option.flatMap(field => jsonGetPath(field, tail))
    | _ => None
    }
  }
}

let lastExn = (l: list<'a>): 'a => l->List.reverse->List.headExn

// TODO: manage enums in schema
let findTitleInSchema = (keys: list<string>, jsonSchema: JSON.t): string => {
  let prop = jsonGetPath(jsonSchema, list{"properties", keys->List.headExn})

  let rec followRef = fields => {
    switch (fields->Dict.get("$ref"), fields->Dict.get("items")) {
    | (Some(ref), _) =>
      let path =
        ref
        ->JSON.Decode.string
        ->Option.getExn
        ->String.split("/")
        ->List.fromArray
        ->List.tailExn
        ->List.concat(list{"properties", keys->lastExn, "title"})
      jsonGetPath(jsonSchema, path)
    | (_, Some(items)) => followRef(items->JSON.Decode.object->Option.getExn)
    | _ => Dict.get(fields, "title")
    }
  }

  switch JSON.Classify.classify(prop) {
  | Object(fields) =>
    switch Dict.get(fields, "title") {
    | None => followRef(fields)
    | Some(title) if title->JSON.Decode.string == Some(" ") || keys->List.length > 1 =>
      followRef(fields)
    | title => title
    }
  | _ => None
  }
  ->Option.flatMap(JSON.Decode.string)
  ->Option.getWithDefault(keys->lastExn)
}

let isDate = (str: string): bool => {
  str->Date.fromString->Date.toString != "Invalid Date"
}

let getNextHeadingLevel = (lvl: HeadingLevel.t): HeadingLevel.t => {
  switch lvl {
  | #Title => #Heading1
  | #Heading1 => #Heading2
  | #Heading2 => #Heading3
  | #Heading3 => #Heading4
  | #Heading4 => #Heading5
  | #Heading5 => #Heading6
  | _ => #Heading6
  }
}
