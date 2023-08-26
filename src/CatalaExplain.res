open Docx2
open FileChild
open Promise

let getUserInputDocSection = (
  ~userInputs: JSON.t,
  ~schema: JSON.t,
  ~uiSchema: JSON.t,
): SectionOptions.t => {
  {
    children: [p'({text: "Entrées du programme", heading: HeadingLevel.h1})],
    // ->Array.concat(
    //      UserInputs.fromJSON(~json=userInputs, ~schema, ~uiSchema)->UserInputs.Docx.toFileChild,
    //    ),
  }
}

let getResultDocSection = (explanationSectionMap: Explanations.sectionMap): SectionOptions.t => {
  {
    children: [p'({text: "Résultats du programme", heading: HeadingLevel.h1})],
    // ->Array.concat(
    //      explanationSectionMap->Explanations.Docx.outputToFileChilds,
    //    ),
  }
}

let getExplanationsDocSection = (
  explanationSectionMap: Explanations.sectionMap,
): SectionOptions.t => {
  {
    children: [
      p'({text: "Explications", heading: HeadingLevel.h1}),
      // Paragraph.create'({
      //   children: [
      //     TextRun.create("Vous trouverez ci-dessous les explications détaillées du calcul."),
      //     TextRun.create("Pour chaque "),
      //     TextRun.create'({text: "étape", italics: true}),
      //     TextRun.create(
      //       " vous trouverez une explication de la règle de calcul utilisée, ainsi que les valeurs des variables utilisées et de potentielles sous-étapes nécessaires.",
      //     ),
      //   ],
      // }),
    ],
    // ]->Array.concat(explanationSectionMap->Explanations.Docx.explanationsToFileChilds),
  }
}

type options = {
  title?: string,
  creator?: string,
  description?: string,
  filename: string,
  schema: JSON.t,
  uiSchema: JSON.t,
}

let version = "0.1.0"

let generate = (~opts: options, ~userInputs: JSON.t, ~events: array<CatalaRuntime.event>) => {
  let explanationSectionMap = events->Explanations.fromEvents
  Document.make({
    title: opts.title->Option.getUnsafe,
    creator: opts.creator->Option.getUnsafe,
    description: opts.description->Option.getUnsafe,
    sections: [
      {
        headers: {
          default: Headers.Header.make({
            children: [
              p'({
                alignment: AlignmentType.right,
                children: [
                  TextRun.make(`catala-explain v${version}`),
                  TextRun.make(" - "),
                  TextRun.make(
                    `${Date.now()
                      ->Date.fromTime
                      ->Date.toLocaleDateStringWithLocaleAndOptions("fr-FR", {dateStyle: #short})}`,
                  ),
                ],
              }),
            ],
          }),
        },
        footers: {
          default: Headers.Footer.make({
            children: [
              p'({
                alignment: AlignmentType.right,
                children: [
                  TextRun.make'({
                    children: [
                      TextRun.pageNumber(PageNumber.current),
                      TextRun.string(" / "),
                      TextRun.pageNumber(PageNumber.totalPages),
                    ],
                  }),
                ],
              }),
            ],
          }),
        },
        children: [
          // Paragraph.create'({
          //   text: opts.title->Option.getWithDefault("Explication individuelle du calcul"),
          //   heading: #Title,
          //   alignment: #center,
          // }),
          // Paragraph.create'({
          //   text: opts.description->Option.getUnsafe,
          //   heading: #Heading2,
          //   alignment: #center,
          // }),
          // Paragraph.create'({
          //   text: `Généré le ${Date.now()
          //     ->Date.fromTime
          //     ->Date.toLocaleDateStringWithLocaleAndOptions("fr-FR", {dateStyle: #long})}`,
          //   heading: #Heading4,
          //   alignment: #center,
          // }),
        ],
      },
      getUserInputDocSection(~userInputs, ~schema=opts.schema, ~uiSchema=opts.uiSchema),
      // explanationSectionMap->getResultDocSection,
      // explanationSectionMap->getExplanationsDocSection,
    ],
    styles: {
      default: Styles.default,
      characterStyles: Styles.characterStyles,
    },
  })
  ->Docx2.Packer.toBlob
  ->thenResolve(blob => {
    Console.log2("blob", blob)
    Docx.FileSaver.saveAs(blob, `${opts.filename}.docx`)
  })
  ->ignore
}
