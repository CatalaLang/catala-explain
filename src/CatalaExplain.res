open Docx
open Promise

// let rec eventToFileChild = (event: CatalaRuntime.event) => {
//   open CatalaRuntime
//
//   switch event {
//   | SubScopeCall({sname, sbody}) =>
//     let subScopeName = Utils.getSubScopeId(sname)
//     let subScopeParagaph = Paragraph.create'({
//       children: [
//         Bookmark.create({
//           id: subScopeName,
//           children: [TextRun.create("SubScopeCall: " ++ subScopeName)],
//         }),
//       ],
//     })
//
//     [subScopeParagaph]->Array.concat(sbody->List.toArray->Array.flatMap(eventToFileChild))
//   | _ => []
//   }
// }

let getUserInputDocSection = (~userInputs: JSON.t, ~jsonSchema: JSON.t): Document.section => {
  {
    children: [
      Paragraph.create'({text: "Entrées du programme", heading: #Heading1}),
    ]->Array.concat(
      UserInputs.fromJSON(~json=userInputs, ~schema=jsonSchema)->UserInputs.Docx.toFileChild,
    ),
  }
}

let getResultDocSection = (explanationSectionMap: Explanations.sectionMap): Document.section => {
  {
    children: [
      Paragraph.create'({text: "Résultats du programme", heading: #Heading1}),
    ]->Array.concat(explanationSectionMap->Explanations.Docx.outputToFileChilds),
  }
}

let getExplanationsDocSection = (
  explanationSectionMap: Explanations.sectionMap,
): Document.section => {
  {
    children: [
      Paragraph.create'({text: "Explications", heading: #Heading1}),
      Paragraph.create'({
        children: [
          TextRun.create("Vous trouverez ci-dessous les explications détaillées du calcul."),
          TextRun.create("Pour chaque "),
          TextRun.create'({text: "étape", italics: true}),
          TextRun.create(
            " vous trouverez une explication de la règle de calcul utilisée, ainsi que les valeurs des variables utilisées et de potentielles sous-étapes nécessaires.",
          ),
        ],
      }),
    ]->Array.concat(explanationSectionMap->Explanations.Docx.explanationsToFileChilds),
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
  let explanationSectionMap = events->Explanations.fromEvents
  Document.create({
    title: opts.title->Option.getUnsafe,
    creator: opts.creator->Option.getUnsafe,
    description: opts.description->Option.getUnsafe,
    sections: [
      // {
      //   children: [
      //     Paragraph.create'({
      //       text: opts.title->Option.getWithDefault("Explication individuelle du calcul"),
      //       heading: #Title,
      //       alignment: #center,
      //     }),
      //     Paragraph.create'({
      //       text: opts.description->Option.getUnsafe,
      //       heading: #Heading2,
      //       alignment: #center,
      //     }),
      //   ],
      // },
      getUserInputDocSection(
        ~userInputs,
        ~jsonSchema=opts.jsonSchema->Option.getWithDefault(JSON.Encode.null),
      ),
      explanationSectionMap->getResultDocSection,
      explanationSectionMap->getExplanationsDocSection,
    ],
    styles: {
      default: Styles.default,
      characterStyles: Styles.characterStyles,
    },
  })
  ->Packer.toBlob
  ->thenResolve(blob => {
    FileSaver.saveAs(blob, `${opts.filename}.docx`)
  })
  ->ignore
}
