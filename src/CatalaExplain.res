open Docx
open Promise

let getUserInputDocSection = (
  ~userInputs: JSON.t,
  ~schema: JSON.t,
  ~uiSchema: JSON.t,
): Document.section => {
  {
    children: [
      Paragraph.create'({text: "Entrées du programme", heading: #Heading1}),
    ]->Array.concat(
      UserInputs.fromJSON(~json=userInputs, ~schema, ~uiSchema)->UserInputs.Docx.toFileChild,
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
      // Paragraph.create'({text: "Explications", heading: #Heading1}),
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
    ]->Array.concat(explanationSectionMap->Explanations.Docx.explanationsToFileChilds),
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
  Document.create({
    title: opts.title->Option.getUnsafe,
    creator: opts.creator->Option.getUnsafe,
    description: opts.description->Option.getUnsafe,
    sections: [
      {
        headers: {
          default: Header.create({
            children: [
              Paragraph.create'({
                alignment: #right,
                children: [
                  TextRun.create(`catala-explain v${version}`),
                  TextRun.create(" - "),
                  TextRun.create(
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
          default: Footer.create({
            children: [
              Paragraph.create'({
                alignment: #right,
                children: [
                  TextRun.create'({
                    children: [
                      TextRun.Children.pageNumber(PageNumber.current),
                      TextRun.Children.string(" / "),
                      TextRun.Children.pageNumber(PageNumber.totalPages),
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
      // getUserInputDocSection(~userInputs, ~schema=opts.schema, ~uiSchema=opts.uiSchema),
      // explanationSectionMap->getResultDocSection,
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
