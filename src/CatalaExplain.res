open Docx
open Promise
open Utils

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

let userInputsToFileChild = (userInputs: JSON.t, jsonSchema: JSON.t): array<fileChild> => {
  let primitiveToString = (input: JSON.t) => {
    switch JSON.Classify.classify(input) {
    | Bool(b) =>
      // TODO: manage the language
      b ? "vrai" : "faux"
    | Number(f) => f->Float.toString
    | String(s) => s
    | Null | Object(_) | Array(_) => "Aucune entrée"
    }
  }
  let rec aux = (userInputs: JSON.t, level: int) => {
    switch JSON.Classify.classify(userInputs) {
    | Object(fields) =>
      fields
      ->Dict.toArray
      ->Array.flatMap(((key, value)) => {
        let inputName = key->findTitleInSchema(jsonSchema)
        if isPrimitive(value) || isEmpty(value) {
          [
            Paragraph.create'({
              bullet: {level: level},
              children: [
                TextRun.create(`${inputName} : `),
                TextRun.create'({
                  text: primitiveToString(value),
                  font: "Fira Mono",
                  bold: isPrimitive(value),
                  italics: isEmpty(value),
                }),
              ],
            }),
          ]
        } else {
          [
            Paragraph.create'({
              children: [TextRun.create(`${inputName} : `)],
              bullet: {level: level},
            }),
          ]->Array.concat(aux(value, level + 1))
        }
      })
    | Array(elems) => elems->Array.flatMap(elem => aux(elem, level))
    | _ => failwith("should not happen")
    }
  }
  aux(userInputs, 0)
}

type options = {
  title?: string,
  creator?: string,
  description?: string,
  filename: string,
  jsonSchema?: JSON.t,
}

let generate = (~opts: options, ~userInputs: JSON.t, ~events: array<CatalaRuntime.event>) => {
  Document.create({
    title: opts.title->Option.getUnsafe,
    creator: opts.creator->Option.getUnsafe,
    description: opts.description->Option.getUnsafe,
    sections: [
      // {
      //   children: [
      //     Paragraph.create'({
      //       text: opts.title->Option.getWithDefault("Explication individuelle du calcul"),
      //       heading: HeadingLevel.title,
      //       alignment: AlignmentType.center,
      //     }),
      //     Paragraph.create'({
      //       text: opts.description->Option.getUnsafe,
      //       heading: HeadingLevel.h2,
      //       alignment: AlignmentType.center,
      //     }),
      //   ],
      // },
      {
        children: [
          Paragraph.create'({text: "Entrées du programme", heading: HeadingLevel.h1}),
        ]->Array.concat(
          userInputsToFileChild(
            userInputs,
            opts.jsonSchema->Option.getWithDefault(JSON.Encode.null),
          ),
        ),
      },
      {
        children: events->Array.flatMap(eventToFileChild),
      },
    ],
  })
  ->Packer.toBlob
  ->thenResolve(blob => {
    FileSaver.saveAs(blob, `${opts.filename}.docx`)
  })
  ->ignore
}
