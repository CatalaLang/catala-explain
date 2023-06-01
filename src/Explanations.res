open CatalaRuntime

/** Manages to generates unique scope call name */
module SubScopeName = {
  module Id = UId.Int.Make()

  type t = {
    id: Id.t,
    name: string,
    infos: information,
  }

  let get = (infos: information): t => {
    let id = Id.fresh()
    let name = Utils.getSubScopeId(infos)
    {id, name, infos}
  }
}

module SectionId = UId.Int.Make()

type rec section = {
  id: SectionId.t,
  parent?: SectionId.t,
  title: string,
  inputs: array<var_def>,
  outputs: array<var_def>,
  explanations: array<explanation>,
}
and explanation = Def(var_def) | Ref(SectionId.t)

type sectionMap = Map.t<SectionId.t, section>

let getOutputs = (events: array<event>): array<var_def> => {
  events->Array.filterMap(event => {
    switch event {
    | VarComputation({io: {io_output}} as var_def) if io_output == true => Some(var_def)
    | _ => None
    }
  })
}

type parseCtx = {
  sections: sectionMap,
  currentId: SectionId.t,
}

let rec parseExplanations = (events: array<event>, ctx: parseCtx): array<explanation> => {
  events->Array.map(event => {
    switch event {
    | VarComputation(var_def) => Def(var_def)
    | SubScopeCall({sname, inputs, sbody}) => {
        let id = SectionId.fresh()
        let title = Utils.getSectionTitle(sname)
        let explanations = sbody->List.toArray->Array.reverse
        let outputs = explanations->getOutputs
        // TODO: remove outputs from explanations?
        let section = {
          id,
          parent: ctx.currentId,
          title,
          inputs: List.toArray(inputs),
          outputs,
          explanations: explanations->parseExplanations({...ctx, currentId: id}),
        }
        ctx.sections->Map.set(id, section)
        Ref(id)
      }
    | FunCall(_) =>
      // TODO: handle function call
      Ref(SectionId.root)
    }
  })
}

let parseRoot = (ctx: parseCtx, events: array<event>): section => {
  let explanations = events->Array.reverse
  let outputs = explanations->getOutputs
  // A program should always have an output
  let firstOutput = outputs->Array.get(0)->Option.getExn
  {
    id: SectionId.root,
    title: firstOutput.name->Utils.getSectionTitle,
    inputs: [],
    outputs,
    explanations: explanations->parseExplanations(ctx),
  }
}

let fromEvents = (events: array<event>): sectionMap => {
  let sections = Map.make()
  let root = parseRoot({sections, currentId: SectionId.root}, events)
  sections->Map.set(root.id, root)
  sections
}

module Docx = {
  open Docx

  let linkToSection = (id: SectionId.t, _title: string): Docx.paragraphChild => {
    InternalHyperlink.create({
      anchor: `section-${id->Int.toString}`,
      children: [
        TextRun.create'({
          text: `n°${id->Int.toString}`,
          style: "Hyperlink",
        }),
      ],
    })
  }

  let bookmarkSection = (id: SectionId.t, title: string): Docx.paragraphChild => {
    Bookmark.create({
      id: `section-${id->Int.toString}`,
      children: [
        TextRun.create'({
          text: `Étape n°${id->Int.toString} : ${title}`,
        }),
      ],
    })
  }

  let isLitLoggedValue = (val: LoggedValue.t): bool => {
    switch val {
    | Enum(_, (_, val)) if val != Unit => false
    | Struct(_, l) if l->List.length != 0 => false
    | Array(l) if l->Array.length != 0 => false
    | Unembeddable => // TODO: handle unembeddable, which are functions and other stuff
      false
    | _ => true
    }
  }

  let litLoggedValueToParagraphChild = (val: LoggedValue.t): paragraphChild => {
    switch val {
    | Bool(b) =>
      // TODO: manage the language
      TextRun.create'({
        text: b ? "vrai" : "faux",
        style: "BooleanLiteral",
      })
    | Money(m) =>
      TextRun.create'({
        text: `${m->Float.toString}€`,
        style: "NumberLiteral",
      })
    | Decimal(f) =>
      TextRun.create'({
        text: f->Float.toString,
        style: "NumberLiteral",
      })
    | Integer(i) =>
      TextRun.create'({
        text: i->Int.toString,
        style: "NumberLiteral",
      })
    | Date(d) =>
      TextRun.create'({
        text: d
        ->Date.fromString
        ->Date.toLocaleDateStringWithLocaleAndOptions("fr-FR", {dateStyle: #long}),
        style: "DateLiteral",
      })
    | Duration(d) =>
      TextRun.create'({
        text: d,
        style: "DateLiteral",
      })
    | Enum(_, (name, Unit)) =>
      TextRun.create'({
        text: name,
        style: "EnumLiteral",
      })
    | Array([]) =>
      TextRun.create'({
        text: "[]",
        style: "ArrayLiteral",
      })
    | _ => Js.Exn.raiseError(`Should be a literal logged value`)
    }
  }

  let rec loggedValueToFileChilds = (~level: int=0, val: LoggedValue.t): array<fileChild> => {
    switch val {
    | Enum(_, (_, val)) =>
      // [
      //   Paragraph.create'({
      //     text: name,
      //     heading: lvl,
      //   }),
      /* ]-> Array.concat( */
      val->loggedValueToFileChilds(~level)
    | Struct(_, l) =>
      // [
      //   Paragraph.create'({
      //     text: infos->Utils.getSubScopeId,
      //     heading: lvl,
      //   }),
      // ]->Array.concat(
      l
      ->List.toArray
      ->Array.filter(((_, value)) => Utils.loggedValueIsEmbeddable(value))
      ->Array.sort(((_, v1), (_, v2)) => Utils.loggedValueCompare(v1, v2))
      ->Array.flatMap(((field, value)) => {
        switch value {
        | Array([]) => []
        | v if v->isLitLoggedValue => [
            Paragraph.create'({
              children: [TextRun.create(`${field} : `), value->litLoggedValueToParagraphChild],
              bullet: {level: level},
            }),
          ]
        | _ =>
          [
            Paragraph.create'({
              text: `${field} : `,
              bullet: {level: level},
            }),
          ]->Array.concat(value->loggedValueToFileChilds(~level))
        }
      })
    // )
    | Array(val) => val->Array.flatMap(loggedValueToFileChilds(_, ~level=level + 1))
    | _ => []
    }
  }

  let varDefToFileChilds = ({name, value, pos}: var_def): array<fileChild> => {
    if value->isLitLoggedValue {
      [
        Paragraph.create'({
          children: [
            TextRun.create(`La variable `),
            TextRun.create'({
              text: `${name->Utils.lastExn}`,
              style: "VariableName",
            }),
            TextRun.create(` vaut `),
            value->litLoggedValueToParagraphChild,
            TextRun.create(`. `),
            pos->Option.mapWithDefault(TextRun.create(""), Utils.getLinkToSourcePos),
          ],
        }),
      ]
    } else {
      [
        Paragraph.create'({
          children: [
            TextRun.create(`La variable `),
            TextRun.create'({
              text: `${name->Utils.lastExn}`,
              style: "VariableName",
            }),
            TextRun.create(` (de type `),
            TextRun.create'({
              text: value->Utils.loggedValueKindToText,
              italics: true,
            }),
            TextRun.create(`) `),
            TextRun.create(` vaut : `),
            pos->Option.mapWithDefault(TextRun.create(""), Utils.getLinkToSourcePos),
          ],
        }),
      ]->Array.concat(value->loggedValueToFileChilds)
    }
  }

  let informationToParagraphChilds = (infos: CatalaRuntime.information): array<paragraphChild> => {
    let length = infos->List.length
    infos
    ->List.toArray
    ->Array.mapWithIndex((info, i) => {
      TextRun.create'({
        text: info,
        style: i == length - 1 ? "VariableName" : "ScopeName",
      })
    })
  }

  @raises(Error.t)
  let outputToFileChilds = (explanationSectionMap: sectionMap): array<fileChild> => {
    let {outputs, explanations} =
      explanationSectionMap
      ->Map.get(SectionId.root)
      ->Utils.getJsErr("Root section not found in [explanationSectionMap]")
    let refs = explanations->Array.filter(explanation => {
      switch explanation {
      | Ref(_) => true
      | _ => false
      }
    })
    // NOTE(@EmileRolley): I assume here that there are only one output for one program
    let output = outputs->Array.get(0)
    let nbRefs = refs->Array.length
    [
      Paragraph.create'({
        children: output
        ->Option.map(output => [
          TextRun.create(`La valeur calculée par le programme est `),
          TextRun.create'({
            text: output.name->List.reverse->List.headExn,
            style: "VariableName",
          }),
          TextRun.create(` et vaut `),
          if output.value->isLitLoggedValue {
            output.value->litLoggedValueToParagraphChild
          } else {
            TextRun.create("TODO (non-literal value)")
          },
          TextRun.create("."),
        ])
        ->Option.getWithDefault([]),
      }),
      Paragraph.create'({
        children: [
          if nbRefs == 1 {
            TextRun.create(`La valeur a été calculée à partir de l'étape de calcul `)
          } else {
            TextRun.create(`La valeur a été calculée à partir des étapes de calculs `)
          },
        ]->Array.concat(
          refs
          ->Array.mapWithIndex((expl, i) => {
            let isLast = i == nbRefs - 1
            switch expl {
            | Ref(id) => {
                let {title} =
                  explanationSectionMap
                  ->Map.get(id)
                  ->Utils.getJsErr(
                    `Section ${id->Int.toString} not found in [explanationSectionMap]`,
                  )
                [
                  linkToSection(id, title),
                  if isLast {
                    TextRun.create(".")
                  } else {
                    TextRun.create(", ")
                  },
                ]
              }
            | _ => []
            }
          })
          ->Array.flat,
        ),
      }),
    ]
  }

  let explanationsToFileChilds = (explanationSectionMap: sectionMap): array<fileChild> => {
    explanationSectionMap
    ->Map.entries
    ->Iterator.toArray
    ->Array.flatMap(((id, {title, inputs, outputs, explanations})) => {
      let inputParagraphs = [
        Paragraph.create'({
          heading: #Heading3,
          children: [
            TextRun.create("Entrées utilisées pour l'étape de calcul "),
            linkToSection(id, title),
          ],
        }),
      ]->Array.concat(
        inputs
        ->Array.filter(({value}) => Utils.loggedValueIsEmbeddable(value))
        ->Array.sort((a, b) => Utils.loggedValueCompare(a.value, b.value))
        ->Array.flatMap(varDefToFileChilds(_)),
      )
      let outputParagraphs = [
        Paragraph.create'({
          heading: #Heading3,
          children: [
            TextRun.create(
              outputs->Array.length > 1
                ? "Valeurs calculées dans l'étape de calcul "
                : "Valeur calculée dans l'étape de calcul ",
            ),
            linkToSection(id, title),
          ],
        }),
      ]->Array.concat(outputs->Array.flatMap(varDefToFileChilds))
      let explanationsParagraphs = [
        Paragraph.create'({
          heading: #Heading3,
          children: [
            TextRun.create("Explications pour l'étape de calcul "),
            linkToSection(id, title),
          ],
        }),
        Paragraph.create'({
          children: explanations->Array.map(expl =>
            switch expl {
            | Ref(id) => {
                let section =
                  explanationSectionMap
                  ->Map.get(id)
                  ->Utils.getJsErr(
                    `Section ${id->Int.toString} not found in [explanationSectionMap]`,
                  )
                InternalHyperlink.create({
                  anchor: `section-${id->Int.toString}`,
                  children: [
                    TextRun.create'({
                      text: `${section.title} - ${id->Int.toString}`,
                      style: "VariableName",
                    }),
                  ],
                })
              }
            | _ => TextRun.create("")
            }
          ),
        }),
      ]
      if id == SectionId.root {
        []
      } else {
        [
          Paragraph.create'({
            heading: #Heading2,
            children: [bookmarkSection(id, title)],
          }),
        ]
        ->Array.concat(inputParagraphs)
        ->Array.concat(outputParagraphs)
        ->Array.concat(explanationsParagraphs)
      }
    })
  }
}
