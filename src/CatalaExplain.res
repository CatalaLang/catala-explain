open Docx
open Promise
open FileChild

type sectionInfos = {
  name: string,
  id: string,
}

let sections = [
  {name: "Entrées du programme", id: "user-inputs"},
  {name: "Résultats du programme", id: "result"},
  {name: "Explications", id: "explanations"},
]

let getTocSection = (_explanationSectionMap): SectionOptions.t => {
  {
    children: [p'({text: "Table des matières", heading: #Heading1})]->Array.concat(
      sections->Array.map(({name, id}) => {
        p'({
          children: [
            TextRun.make'({
              children: [TextRun.string(name)],
            }),
            TextRun.make'({children: [Tab.make(), TextRun.string("")]}),
            PageReference.make(id),
          ],
          heading: #Heading6,
          tabStops: [
            {
              type_: #center,
              position: TabStopDefinition.maxPosition,
              leader: #dot,
            },
          ],
        })
      }),
    ),
  }
}

let getSectionHeading = i => {
  let {name, id} = sections->Array.getUnsafe(i)
  p'({
    heading: #Heading1,
    children: [
      Bookmark.make({
        id,
        children: [TextRun.make'({text: name})],
      }),
    ],
  })
}

let getUserInputDocSection = (~userInputs, ~schema, ~keysToIgnore): SectionOptions.t => {
  {
    children: [
      getSectionHeading(0),
      UserInputs.parseVarDefs(~json=userInputs, ~schema, ~keysToIgnore)->UserInputs.toTable,
    ],
  }
}

let getResultDocSection = (~selectedOutput, explanationSectionMap): SectionOptions.t => {
  {
    children: [getSectionHeading(1)]->Array.concat(
      explanationSectionMap->Explanations.Docx.outputToFileChilds(~selectedOutput),
    ),
  }
}

let getExplanationsDocSection = (
  explanationSectionMap: Explanations.sectionMap,
): SectionOptions.t => {
  {
    children: [
      getSectionHeading(2),
      p'({
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

let version = "0.1.1"

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
              p'({
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
              p'({
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
          p'({
            text: opts.title->Option.getWithDefault("Explication individuelle du calcul"),
            heading: #Title,
            alignment: #center,
          }),
          p'({
            text: opts.description->Option.getUnsafe,
            heading: #Heading2,
            alignment: #center,
          }),
        ],
      },
      getTocSection(explanationSectionMap),
      getUserInputDocSection(~userInputs, ~schema=opts.schema, ~keysToIgnore=opts.keysToIgnore),
      getResultDocSection(~selectedOutput=opts.selectedOutput, explanationSectionMap),
      getExplanationsDocSection(explanationSectionMap),
    ],
  })
  ->Docx.Packer.toBlob
  ->thenResolve(blob => {
    FileSaver.saveAs(blob, `${opts.filename}.docx`)
  })
  ->ignore
}
