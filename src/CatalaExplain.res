open Docx
open Promise

let rec eventToFileChild = (event: CatalaRuntime.event) => {
  open CatalaRuntime

  switch event {
  | SubScopeCall({sname, sbody}) =>
    [Paragraph.create("SubScopeCall: " ++ sname->List.toArray->Array.joinWith(" "))]->Array.concat(
      sbody->List.toArray->Array.flatMap(eventToFileChild),
    )
  | _ => []
  }
}

let isPrimitive = (input: JSON.t) => {
  switch JSON.Classify.classify(input) {
  | Object(_) | Array(_) => false
  | _ => true
  }
}

let userInputsToFileChild = (userInputs: JSON.t) => {
  let primitiveToString = (input: JSON.t) => {
    switch JSON.Classify.classify(input) {
    | Bool(b) =>
      // TODO: manage the language
      b ? "vrai" : "faux"
    | Number(f) => f->Float.toString
    | String(s) => s
    | Null => ""
    | _ => failwith("Not a primitive")
    }
  }
  let rec aux = (userInputs: JSON.t) => {
    Console.log2("aux.userInputs: ", userInputs)
    switch JSON.Classify.classify(userInputs) {
    | Object(fields) =>
      fields
      ->Dict.toArray
      ->Array.flatMap(((key, value)) => {
        if isPrimitive(value) {
          [TextRun.create(key ++ " : " ++ primitiveToString(value))]
        } else {
          Console.log2("got: ", value)
          [TextRun.create(key ++ " : ")]->Array.concat(aux(value))
        }
      })
    | Array([]) => [TextRun.create("Aucune entrée")]
    | Array(elems) => elems->Array.flatMap(aux)
    | _ => failwith("should not happen")
    }
  }
  aux(userInputs)
}

let generate = (~userInputs: JSON.t, ~events: array<CatalaRuntime.event>) => {
  Console.log(userInputs)
  let doc = Document.create({
    sections: [
      {
        children: [
          Paragraph.create'({text: "Entrées du programme", heading: HeadingLevel.h1}),
          Paragraph.create'({children: userInputsToFileChild(userInputs)}),
        ],
      },
      {
        children: events->Array.flatMap(eventToFileChild),
      },
    ],
  })
  doc
  ->Packer.toBlob
  ->thenResolve(blob => {
    Console.log(blob)
    FileSaver.saveAs(blob, "example.docx")
  })
  ->ignore
}
