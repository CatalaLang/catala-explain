open Docx
open Promise

let getUserInputDocSection = (~userInputs, ~schema, ~uiSchema, ~keysToIgnore): SectionOptions.t => {
  {
    children: [
      FileChild.p'({text: "Entrées du programme", heading: #Heading1}),
      UserInputs.parseVarDefs(
        ~json=userInputs,
        ~schema,
        ~uiSchema,
        ~keysToIgnore,
      )->UserInputs.toTable,
    ],
  }
}

let getResultDocSection = (~selectedOutput, explanationSectionMap): SectionOptions.t => {
  {
    children: [FileChild.p'({text: "Résultats du programme", heading: #Heading1})]->Array.concat(
      explanationSectionMap->Explanations.Docx.outputToFileChilds(~selectedOutput),
    ),
  }
}

let getExplanationsDocSection = (
  explanationSectionMap: Explanations.sectionMap,
): SectionOptions.t => {
  {
    children: [
      FileChild.p'({text: "Explications", heading: #Heading1}),
      FileChild.p'({
        children: [
          TextRun.make("Vous trouverez ci-dessous les explications détaillées du calcul."),
          TextRun.make("Pour chaque "),
          TextRun.make'({text: "étape", italics: true}),
          TextRun.make(
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
  schema: JSON.t,
  uiSchema: JSON.t,
  keysToIgnore: array<string>,
  selectedOutput: CatalaRuntime.information,
}

let version = "0.1.0"

let generate = (~userInputs, ~events, ~opts) => {
  let explanationSectionMap = events->Explanations.fromEvents
  Document.make({
    title: opts.title->Option.getUnsafe,
    creator: opts.creator->Option.getUnsafe,
    description: opts.description->Option.getUnsafe,
    styles: {
      default: Styles.default,
      characterStyles: Styles.characterStyles,
    },
    numbering: {
      config: [
        {
          reference: "decimal",
          levels: [
            {
              level: 0,
              format: #decimal,
              text: "%1.",
              alignment: #right,
            },
          ],
        },
      ],
    },
    sections: [
      {
        headers: {
          default: Headers.Header.make({
            children: [
              FileChild.p'({
                alignment: #right,
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
              FileChild.p'({
                alignment: #right,
                children: [
                  TextRun.make'({
                    children: [
                      TextRun.pageNumber(#CURRENT),
                      TextRun.string(" / "),
                      TextRun.pageNumber(#TOTAL_PAGES),
                    ],
                  }),
                ],
              }),
            ],
          }),
        },
        children: [
          FileChild.p'({
            text: opts.title->Option.getWithDefault("Explication individuelle du calcul"),
            heading: #Title,
            alignment: #center,
          }),
          FileChild.p'({
            text: opts.description->Option.getUnsafe,
            heading: #Heading2,
            alignment: #center,
          }),
        ],
      },
      getUserInputDocSection(
        ~userInputs,
        ~schema=opts.schema,
        ~uiSchema=opts.uiSchema,
        ~keysToIgnore=opts.keysToIgnore,
      ),
      explanationSectionMap->getResultDocSection(~selectedOutput=opts.selectedOutput),
      explanationSectionMap->getExplanationsDocSection,
    ],
  })
  ->Docx.Packer.toBlob
  ->thenResolve(blob => {
    FileSaver.saveAs(blob, `${opts.filename}.docx`)
  })
  ->ignore
}
