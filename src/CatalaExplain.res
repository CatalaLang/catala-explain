open Docx
open Promise
open Utils

let litToStyledTextRun = (lit: UserInputs.t): Docx.paragraphChild => {
  switch lit {
  | LitBool(b) =>
    // TODO: manage the language
    TextRun.create'({
      text: b ? "vrai" : "faux",
      style: "BooleanLiteral",
    })
  | LitNumber(f) =>
    TextRun.create'({
      text: f->Float.toString,
      style: "NumberLiteral",
    })
  | LitDate(d) =>
    TextRun.create'({
      text: d
      ->Date.fromString
      ->Date.toLocaleDateStringWithLocaleAndOptions("fr-FR", {dateStyle: #long}),
      style: "DateLiteral",
    })
  | LitString(s) =>
    TextRun.create'({
      text: s,
      style: "StringLiteral",
    })
  | LitEnum(name) =>
    TextRun.create'({
      text: name,
      style: "EnumLiteral",
    })
  | LitNull => TextRun.create'({text: "Aucune entrée", style: "EmptyLiteral"})
  | _ => failwith("invalid user inputs in [litToStyledTextRun]")
  }
}

let userInputsToFileChild = (userInputs: UserInputs.t, _jsonSchema: JSON.t): array<fileChild> => {
  let rec aux = (userInputs: UserInputs.t, level: HeadingLevel.t) => {
    switch userInputs {
    | Section({title, items}) =>
      [
        Paragraph.create'({
          children: [TextRun.create(title)],
          heading: level,
        }),
      ]->Array.concat(aux(items, getNextHeadingLevel(level)))
    | Array(inputs) =>
      // Need to have dot list ?
      inputs->Array.flatMap(input => aux(input, getNextHeadingLevel(level)))
    | Fields(items) =>
      items
      ->Array.sort((a, b) => {
        // Display section last
        switch (a, b) {
        | (Section(_), _) | (_, Section(_)) => -1
        | (Array(_), _) | (_, Array(_)) => -1
        | _ => 1
        }
      })
      ->Array.flatMap(input => aux(input, getNextHeadingLevel(level)))
    | Field({name, value}) if UserInputs.isLiteral(value) => [
        Paragraph.create'({
          children: [TextRun.create(`${name} : `), litToStyledTextRun(value)],
        }),
      ]
    | Field({value}) if UserInputs.isEmpty(value) => []
    | Field({name, value}) =>
      [
        Paragraph.create'({
          children: [TextRun.create(name)],
          heading: level,
        }),
      ]->Array.concat(aux(value, getNextHeadingLevel(level)))
    | _ => failwith("invalid user inputs in [userInputsToFileChild]")
    }
  }
  aux(userInputs, HeadingLevel.h2)
}

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

type options = {
  title?: string,
  creator?: string,
  description?: string,
  filename: string,
  jsonSchema?: JSON.t,
}

let generate = (~opts: options, ~userInputs: JSON.t, ~events: array<CatalaRuntime.event>) => {
  Console.log2("userInputs", userInputs)
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
            UserInputs.fromJSON(
              userInputs,
              opts.jsonSchema->Option.getWithDefault(JSON.Encode.null),
            ),
            // Should not be needed
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
          id: "EnumLiteral",
          name: "EnumLiteral",
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
