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

let valueToStyledTextRun = (value: JSON.t): Docx.paragraphChild => {
  switch JSON.Classify.classify(value) {
  | Bool(b) =>
    // TODO: manage the language
    TextRun.create'({
      text: b ? "vrai" : "faux",
      style: "BooleanLiteral",
    })
  | Number(f) =>
    TextRun.create'({
      text: f->Float.toString,
      style: "NumberLiteral",
    })
  | String(s) =>
    if isDate(s) {
      TextRun.create'({
        text: s
        ->Date.fromString
        ->Date.toLocaleDateStringWithLocaleAndOptions("fr-FR", {dateStyle: #long}),
        style: "DateLiteral",
      })
    } else {
      TextRun.create'({
        text: s,
        style: "StringLiteral",
      })
    }
  | Null | Object(_) | Array(_) => TextRun.create'({text: "Aucune entrée", style: "EmptyLiteral"})
  }
}

let userInputsToFileChild = (userInputs: JSON.t, jsonSchema: JSON.t): array<fileChild> => {
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
              children: [TextRun.create(`${inputName} : `), valueToStyledTextRun(value)],
              bullet: {level: level},
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
    styles: {
      characterStyles: [
        {
          id: "BooleanLiteral",
          name: "BooleanLiteral",
          basedOn: "Normal",
          next: "Normal",
          quickFormat: true,
          run: {
            font: "Fira Mono",
            bold: true,
            color: "BA2121",
          },
        },
        {
          id: "NumberLiteral",
          name: "NumberLiteral",
          basedOn: "Normal",
          next: "Normal",
          quickFormat: true,
          run: {
            font: "Fira Mono",
            bold: true,
            color: "008000",
          },
        },
        {
          id: "StringLiteral",
          name: "StringLiteral",
          basedOn: "Normal",
          next: "Normal",
          quickFormat: true,
          run: {
            font: "Fira Mono",
            bold: true,
            color: "BB0066",
          },
        },
        {
          id: "DateLiteral",
          name: "DateLiteral",
          basedOn: "Normal",
          next: "Normal",
          quickFormat: true,
          run: {
            font: "Fira Mono",
            bold: true,
            color: "0000FF",
          },
        },
        {
          id: "EmptyLiteral",
          name: "EmptyLiteral",
          basedOn: "Normal",
          next: "Normal",
          quickFormat: true,
          run: {
            font: "Fira Mono",
            italics: true,
          },
        },
      ],
    },
  })
  ->Packer.toBlob
  ->thenResolve(blob => {
    FileSaver.saveAs(blob, `${opts.filename}.docx`)
  })
  ->ignore
}
