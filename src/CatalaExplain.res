open Docx
open FileChild

type sectionInfos = {
  name: string,
  id: string,
}

let sections = [
  {name: "Entrées du calcul", id: "user-inputs"},
  {name: "Résultats du calcul", id: "result"},
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
      ->Array.toSorted(((id, _), (id', _)) => Explanations.SectionId.compare(id, id'))
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

let getUserInputDocSection = (~userInputs, ~schema): SectionOptions.t => {
  {
    children: [
      getSectionHeading(0),
      p'({
        text: "Cette section rappelle quelles sont les entrées du calcul telles que \
vous les avez renseignées dans le formulaire. Il est important de vérifier \
que ces informations soient exactes, puisqu'une erreur de votre part dans \
le remplissage du formulaire mènera très probablement à une erreur dans \
la détermination du résultat du calcul.",
        alignment: #both,
        spacing: {before: Styles.Spacing.small, after: Styles.Spacing.small},
      }),
      UserInputs.parseVarDefs(~json=userInputs, ~schema)->UserInputs.toTable,
    ],
  }
}

let getResultDocSection = (explanationSectionMap): SectionOptions.t => {
  {
    children: [getSectionHeading(1)]->Array.concat(
      explanationSectionMap->Explanations.Docx.outputToFileChilds,
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
        spacing: {before: Styles.Spacing.small, after: Styles.Spacing.small},
        alignment: #both,
        children: [
          TextRun.make("Vous trouverez ci-dessous les explications détaillées du calcul."),
          TextRun.make(" Pour chaque "),
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
  sourcesURL?: string,
}

let version = "0.2.2"

let generate = (~events, ~userInputs, ~schema, ~opts) => {
  open Docx.Util.Types

  Context.keysToIgnore := opts.keysToIgnore->Option.getOr([])
  Context.sourcesURL := opts.sourcesURL
  Context.selectedOutput := opts.selectedOutput

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
            text: opts.title->Option.getOr("Explication individuelle du calcul"),
            heading: #Title,
            alignment: #center,
          }),
          p'({
            text: opts.description->Option.getUnsafe,
            heading: #Heading3,
            alignment: #center,
            border: {
              top: Styles.solidBorder,
              bottom: Styles.solidBorder,
              left: Styles.solidBorder,
              right: Styles.solidBorder,
            },
            spacing: {before: Styles.Spacing.large, after: Styles.Spacing.medium},
          }),
          switch opts.creator {
          | Some(creator) =>
            p'({
              alignment: #center,
              spacing: {after: Styles.Spacing.medium},
              children: [
                TextRun.make'({
                  text: `Créé depuis ${creator}`,
                  italics: true,
                }),
              ],
            })
          | None => p("")
          },
          p'({
            alignment: #both,
            children: [
              TextRun.make("Cette explication a été générée automatiquement par "),
              Docx.ExternalHyperlink.make({
                link: "https://code.gouv.fr/fr/explicabilite/catala/",
                children: [TextRun.make("Catala")],
              }),
              TextRun.make(
                " à partir des étapes informatiques du calcul. Chaque étape du calcul \
est ainsi justifiée par une référence juridique présente dans le \
code source du programme. Cette explication détaillée n'est pas \
forcément intelligible si vous ne souhaitez pas descendre dans les \
détails du calcul, cependant elle sera utile pour les concepteurs \
de l'algorithme en cas de désaccord avec le résultat du calcul. Ce document \
n'est pas destiné à être imprimé en totalité pour éviter de gâcher du papier ; \
nous vous conseillons de n'imprimer que les entrées et le résultat final du \
programme si vous souhaitez disposer d'une version papier.",
              ),
            ],
            spacing: {after: Styles.Spacing.medium},
          }),
          p'({
            alignment: #both,
            text: "Pour lire l'explication et comprendre quelles sont les étapes \
du calcul ayant mené au résultat, le document commence par rappeler les données
d'entrée du programme qui sont celles que vous avez entrées dans le formulaire,
puis le résultat final du calcul est présenté avec la liste des étapes de \
haut-niveau ayant mené au résultat. Chacune de ces étapes haut-niveau est \
ensuite détaillée, et peut renvoyer à d'autres étapes de bas-niveau, formant \
ainsi un arbre d'explications qui rappelle les \"livres dont vous êtes le héros\". \
À tout moment, n'hésitez pas à revenir à la table des matières pour vous repérer \
dans cette jungle d'étapes qui ensemble, documentent l'intégralité du raisonnement \
juridique effectué pour prendre la décision. À l'intérieur de chaque étape de \
calcul, vous trouverez les entrées et le résultat de cette étape, ainsi que \
le détail des calculs ayant mené au résultat de l'étape. Les détails des \
calculs ainsi que le résultat sont justifiés par des références juridiques \
présentes dans le code source du programme Catala.",
            spacing: {after: Styles.Spacing.medium},
          }),
          p'({
            alignment: #both,
            text: "Bonne lecture, et n'hésitez pas à vous rapprocher d'un \
professionnel qui vous aidera à déchiffrer ce document si besoin !",
          }),
        ],
      },
      getUserInputDocSection(~userInputs, ~schema),
      getResultDocSection(explanationSectionMap),
      getTocSection(explanationSectionMap),
      getExplanationsDocSection(explanationSectionMap),
    ],
  })
}
