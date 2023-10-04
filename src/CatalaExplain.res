open Docx
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

let getTocSection = (explanationSectionMap: Explanations.sectionMap): SectionOptions.t => {
  {
    children: [p'({text: "Table des matières", heading: #Heading1})]->Array.concatMany([
      sections->Array.map(({name, id}) => {
        p'({
          spacing: {
            before: 100.,
            after: 100.,
          },
          children: [
            TextRun.make(name),
            TextRun.make'({children: [Tab.make(), TextRun.string("")]}),
            PageReference.make(id),
          ],
          heading: #Heading5,
          tabStops: [
            {
              type_: #center,
              position: TabStopDefinition.maxPosition,
              leader: #dot,
            },
          ],
        })
      }),
      explanationSectionMap
      ->Map.entries
      ->Iterator.toArray
      ->Array.sort(((id, _), (id', _)) => Explanations.SectionId.compare(id, id'))
      ->Array.filterMap(((id, {scopeName})) => {
        if id == Explanations.SectionId.root {
          None
        } else {
          Some(
            p'({
              spacing: {
                after: 50.0,
                before: 50.0,
              },
              children: [
                TextRun.make'({
                  bold: true,
                  children: [Tab.make(), TextRun.string(`Étape n°${id->Int.toString} : `)],
                }),
              ]->Array.concatMany([
                scopeName->Utils.getSectionTitle(~size=Some(11)),
                [
                  TextRun.make'({children: [Tab.make()]}),
                  id->Utils.getBookmarkId->PageReference.make,
                ],
              ]),
              heading: #Heading6,
              tabStops: [
                {
                  type_: #left,
                  position: 750,
                  leader: #none,
                },
                {
                  type_: #right,
                  position: TabStopDefinition.maxPosition,
                  leader: #dot,
                },
              ],
            }),
          )
        }
      }),
    ]),
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
  keysToIgnore?: array<string>,
  selectedOutput?: CatalaRuntime.information,
}

let version = "0.1.4"

let generate = (~events, ~userInputs, ~schema, ~opts) => {
  open Docx.Util.Types

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
              alignment: #left,
              style: {
                paragraph: {
                  indent: {
                    left: Number.fromFloat(720.),
                    hanging: Number.fromFloat(360.),
                  },
                },
              },
            },
          ],
        },
        {
          reference: "bullet",
          levels: [
            {
              level: 0,
              format: #bullet,
              text: "•",
              alignment: #left,
              style: {
                paragraph: {
                  indent: {
                    left: Number.fromFloat(720.),
                    hanging: Number.fromFloat(360.),
                  },
                },
              },
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
      getUserInputDocSection(
        ~userInputs,
        ~schema,
        ~keysToIgnore=opts.keysToIgnore->Option.getWithDefault([]),
      ),
      getResultDocSection(~selectedOutput=opts.selectedOutput, explanationSectionMap),
      getExplanationsDocSection(explanationSectionMap),
    ],
  })
}
