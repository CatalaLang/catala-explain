open Utils

type rec t =
  | Section({title: string, items: t})
  | Fields(array<t>)
  | Field({name: string, value: t})
  | Array(array<t>)
  | LitEnum(string)
  | LitDate(string)
  | LitString(string)
  | LitNumber(float)
  | LitBool(bool)
  | LitNull

let isLiteral = (t: t): bool => {
  switch t {
  | LitEnum(_) | LitDate(_) | LitString(_) | LitNumber(_) | LitBool(_) | LitNull => true
  | _ => false
  }
}

let rec isEmpty = (t: t): bool => {
  switch t {
  | Section({items}) => isEmpty(items)
  | Field({value}) => isEmpty(value)
  | Fields(items) | Array(items) => items->Array.length == 0
  | _ => false
  }
}

let getKindName = (obj: JSON.t): string => {
  switch JSON.Classify.classify(obj) {
  | Object(items) =>
    switch items->Dict.get("default") {
    | Some(defaultVal) => JSON.Decode.string(defaultVal)->Option.getExn
    | None => failwith("invalid user inputs in [getKindName]")
    }
  | String(s) => s
  | _ => failwith("invalid user inputs in [getKindName]")
  }
}

// let retrieveKindTitleFromSchema = (obj: JSON.t, schema: JSON.t): string => {
//   failwith("TODO")
// }

let rec fromJSON = (json: JSON.t, schema: JSON.t): t => {
  Console.log2("fromJSON", json)
  switch JSON.Classify.classify(json) {
  | Object(items) =>
    switch (items->Dict.get("kind"), items->Dict.get("payload")) {
    | (None, None) =>
      Fields(
        items
        ->Dict.toArray
        ->Array.map(((key, value)) => {
          let name = key->findTitleInSchema(schema)
          Field({name, value: value->fromJSON(schema)})
        }),
      )
    | (Some(kindVal), None) => LitEnum(getKindName(kindVal))
    | (Some(kindVal), Some(payload)) =>
      Section({title: getKindName(kindVal), items: payload->fromJSON(schema)})
    | _ => failwith("invalid user inputs in [fromJSON]")
    }
  | Array(items) =>
    // TODO: add section for each item
    Array(items->Array.map(item => item->fromJSON(schema)))
  | String(s) =>
    if isDate(s) {
      LitDate(s)
    } else {
      LitString(s)
    }
  | Bool(b) => LitBool(b)
  | Number(f) => LitNumber(f)
  | Null => LitNull
  }
}
