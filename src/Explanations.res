open CatalaRuntime

module SectionId = UId.Int.Make()

type rec section = {
  id: SectionId.t,
  parent: SectionId.t,
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
  let createNewSection = (name, inputs, body, outputs) => {
    let id = SectionId.fresh()
    let title = Utils.getSectionTitle(name)
    let section = {
      id,
      parent: ctx.currentId,
      title,
      inputs: List.toArray(inputs),
      outputs,
      explanations: body->List.toArray->parseExplanations({...ctx, currentId: id}),
    }
    ctx.sections->Map.set(id, section)
    Ref(id)
  }

  events->Array.map(event => {
    switch event {
    | VarComputation(var_def) => Def(var_def)
    | SubScopeCall({sname, inputs, sbody}) => {
        let outputs = sbody->List.toArray->Array.reverse->getOutputs
        createNewSection(sname, inputs, sbody, outputs)
      }
    | FunCall({fun_name, fun_inputs, body, output}) =>
      createNewSection(fun_name, fun_inputs, body, [output])
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
    parent: SectionId.root,
    inputs: [],
    outputs,
    explanations: explanations->parseExplanations(ctx),
  }
}

let fromEvents = (events: array<event>): sectionMap => {
  SectionId.reset()
  let sections = Map.make()
  let root = parseRoot({sections, currentId: SectionId.root}, events)
  sections->Map.set(root.id, root)
  sections
}

module Docx = {
  open Docx

  let linkToSection = (id: SectionId.t): array<Docx.paragraph_child> => {
    let bookmarkId = `section-${id->Int.toString}`

    [
      InternalHyperlink.create({
        anchor: bookmarkId,
        children: [
          TextRun.create'({
            text: `n°${id->Int.toString}`,
            style: "Hyperlink",
          }),
        ],
      }),
      TextRun.create(` (p. `),
      PageReference.create(bookmarkId),
      TextRun.create(`)`),
    ]
  }

  let bookmarkSection = (id: SectionId.t, title: string): Docx.paragraph_child => {
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

  let litLoggedValueToParagraphChild = (val: LoggedValue.t): paragraph_child => {
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
        text: d->Utils.durationToString,
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

  let rec loggedValueToFileChilds = (~level: int=0, val: LoggedValue.t): array<file_child> => {
    switch val {
    | Enum(_, (_, val)) => val->loggedValueToFileChilds(~level)
    | Struct(_, l) =>
      l
      ->List.toArray
      ->Array.filter(((_, value)) => Utils.loggedValueIsEmbeddable(value))
      ->Array.sort(((_, v1), (_, v2)) => Utils.loggedValueCompare(v1, v2))
      ->Array.flatMap(((field, value)) => {
        switch value {
        | Array([]) => []
        | Array(_) =>
          [
            Paragraph.create'({
              children: [
                TextRun.create("Le champ "),
                TextRun.create'({
                  text: field,
                  style: "VariableName",
                }),
                TextRun.create(" est un ensemble contenant : "),
              ],
              bullet: {level: level},
            }),
          ]->Array.concat(value->loggedValueToFileChilds(~level=level + 1))
        | v if v->isLitLoggedValue => [
            Paragraph.create'({
              children: [
                TextRun.create("Le champ "),
                TextRun.create'({
                  text: field,
                  style: "VariableName",
                }),
                TextRun.create(" vaut "),
                value->litLoggedValueToParagraphChild,
              ],
              bullet: {level: level},
            }),
          ]
        | _ =>
          [
            Paragraph.create'({
              children: [
                TextRun.create("Le champ "),
                TextRun.create'({
                  text: field,
                  style: "VariableName",
                }),
                TextRun.create(" vaut : "),
              ],
              bullet: {level: level},
            }),
          ]->Array.concat(value->loggedValueToFileChilds(~level))
        }
      })
    | Array(val) =>
      let id = ref(-1)
      val->Array.flatMap(elem => {
        id := id.contents + 1
        [
          Paragraph.create'({
            children: [TextRun.create(`l'élément n°${id.contents->Int.toString} : `)],
            bullet: {level: level},
          }),
        ]->Array.concat(elem->loggedValueToFileChilds(~level=level + 1))
      })
    | _ => []
    }
  }

  let getLawHeadingInTableRow = (pos: option<CatalaRuntime.sourcePosition>): option<TableRow.t> => {
    switch pos {
    | Some(pos) =>
      TableRow.create({
        children: [
          TableCell.create({
            children: [
              Paragraph.create'({
                children: [Utils.getLawHeadingBreadcrumbsLink(pos)],
                spacing: {after: 40, before: 40},
              }),
            ],
            width: {size: 100.0, _type: #pct},
            borders: {
              top: {
                style: #dotted,
                size: 0.25,
                color: "#000000",
              },
            },
          }),
        ],
        height: {value: NumberOrPositiveUniversalMeasure.number(0.0), rule: #atLeast},
      })->Some
    | None => None
    }
  }

  let litLogValueToCell = (val: LoggedValue.t): TableCell.t => {
    TableCell.create({
      children: [
        Paragraph.create'({
          alignment: #right,
          children: [val->litLoggedValueToParagraphChild],
        }),
      ],
      width: {size: 20.0, _type: #pct},
      borders: {
        bottom: {
          style: #none,
        },
      },
    })
  }

  let getEmptyCellArray = (length: int): array<TableCell.t> => {
    Array.make(
      ~length,
      TableCell.create({
        children: [],
        width: {size: 10.0, _type: #dxa},
        borders: {
          bottom: {
            style: #none,
          },
        },
      }),
    )
  }

  let litVarDefToTableRow = (~depth=1, ~maxDepth, {name, value, pos}: var_def): array<
    TableRow.t,
  > => {
    let columnSpan = maxDepth - depth + 1
    Console.log2("depth", depth)
    Console.log2("maxDepth", maxDepth)
    Console.log2("name", name)
    let varNameCell = TableCell.create({
      children: [
        Paragraph.create'({
          children: [TextRun.create'({text: name->Utils.lastExn, style: "NormalTableCellText"})],
          spacing: {before: 40, after: 40},
        }),
      ],
      columnSpan,
      width: {size: 80.0, _type: #pct},
      borders: {
        bottom: {
          style: #none,
        },
      },
    })

    let varDef = TableRow.create({
      children: (depth - 1)
      ->getEmptyCellArray
      ->Array.concat([varNameCell, value->litLogValueToCell]),
    })

    switch getLawHeadingInTableRow(pos) {
    | Some(breadcrumpsRow) => [varDef, breadcrumpsRow]
    // NOTE(@EmileRolley): should it be possible?
    | None => [varDef]
    }
  }

  let varDefToTableRow = (~depth=1, ~maxDepth, {name, value, pos}: var_def): array<TableRow.t> => {
    let varNameRow = TableRow.create({
      children: getEmptyCellArray(depth - 1)->Array.concat([
        TableCell.create({
          children: [
            Paragraph.create'({
              children: [TextRun.create'({text: name->Utils.lastExn, style: "BoldTableCellText"})],
              spacing: {before: 40, after: 40},
            }),
          ],
          width: {size: 80.0, _type: #pct},
          borders: {
            bottom: {
              style: #none,
            },
          },
        }),
      ]),
    })

    let litValueRows = switch value {
    | Enum(_, (_, val)) => [
        TableRow.create({
          children: getEmptyCellArray(depth)->Array.concat(
            if val->isLitLoggedValue {
              [val->litLogValueToCell]
            } else {
              [
                TableCell.create({
                  children: [Paragraph.create("TODO")],
                  width: {size: 20.0, _type: #pct},
                }),
              ]
            },
          ),
        }),
      ]
    | Struct(_, l) =>
      l
      ->List.toArray
      ->Array.filter(((_, value)) => Utils.loggedValueIsEmbeddable(value))
      ->Array.sort(((_, v1), (_, v2)) => Utils.loggedValueCompare(v1, v2))
      ->Array.flatMap(((field, value)) => {
        switch value {
        | v if v->isLitLoggedValue =>
          litVarDefToTableRow(
            ~depth=depth + 1,
            ~maxDepth,
            {
              name: list{field},
              value: v,
              pos: None,
              // TODO: factorize this
              fun_calls: None,
              io: {io_input: NoInput, io_output: false},
            },
          )
        | _ => [
            TableRow.create({
              children: [
                TableCell.create({
                  children: [Paragraph.create("TODO")],
                  width: {size: 20.0, _type: #pct},
                }),
              ],
            }),
          ]
        }
      })

    | Array(val) => []
    | _ => Js.Exn.raiseError(`Non-literal value is expected.`)
    }

    switch getLawHeadingInTableRow(pos) {
    | Some(breadcrumpsRow) => [varNameRow, breadcrumpsRow]
    // NOTE(@EmileRolley): should it be possible?
    | None => [varNameRow]
    }->Array.concat(litValueRows)
  }

  let varDefToFileChilds = ({name, value, pos}: var_def): array<file_child> => {
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
            pos->Option.mapWithDefault(TextRun.create(""), Utils.getLawHeadingBreadcrumbsLink),
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
            TextRun.create(` ${value->Utils.isArrayLoggedValue ? "est composé de" : "vaut"} : `),
            pos->Option.mapWithDefault(TextRun.create(""), Utils.getLawHeadingBreadcrumbsLink),
          ],
        }),
      ]->Array.concat(value->loggedValueToFileChilds)
    }
  }

  @raises(Error.t)
  let outputToFileChilds = (explanationSectionMap: sectionMap): array<file_child> => {
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
            | Ref(id) =>
              linkToSection(id)->Array.concat([
                if isLast {
                  TextRun.create(".")
                } else {
                  TextRun.create(", ")
                },
              ])
            | _ => []
            }
          })
          ->Array.flat,
        ),
      }),
    ]
  }

  let getMaxInputDepth = (inputs: array<var_def>): int => {
    let rec loggedValueGetMaxDepth = (~depth=0, value: LoggedValue.t): int => {
      switch value {
      | Struct(_, l) =>
        l
        ->List.toArray
        ->Array.map(((name, value)) => {
          let res = value->loggedValueGetMaxDepth(~depth=depth + 1)
          Console.log2(name, res)
          res
        })
        ->Array.reduce(0, (a, b) => Math.Int.max(a, b))
      | Enum(_, (_, _l)) => depth //TODO
      // l->loggedValueGetMaxDepth(~depth=depth + 1)
      | Array(_) => // TODO
        depth
      // l
      // ->Array.map(value => value->loggedValueGetMaxDepth(~depth=depth + 1))
      // ->Array.reduce(0, (a, b) => Math.Int.max(a, b))
      | _ => depth
      }
    }

    inputs
    ->Array.map(({name, value}) => {
      if !(value->isLitLoggedValue) {
        let res = value->loggedValueGetMaxDepth

        // Console.log2(name->List.toArray, res)
        res
      } else {
        0
      }
    })
    ->Array.reduce(0, (a, b) => {
      let max = Math.Int.max(a, b)
      Console.log3(a, b, max)
      max
    })
  }

  let explanationsToFileChilds = (explanationSectionMap: sectionMap): array<file_child> => {
    explanationSectionMap
    ->Map.entries
    ->Iterator.toArray
    ->Array.sort(((id, _), (id', _)) => SectionId.compare(id, id'))
    ->Array.flatMap(((id, {title, inputs, outputs, explanations, parent})) => {
      if id !== 1 {
        []
      } else {
        let maxInputsDepth = inputs->getMaxInputDepth
        let inputTable = Table.create({
          width: {size: 100.0, _type: #pct},
          layout: #fixed,
          alignment: #center,
          rows: [
            TableRow.create({
              tableHeader: true,
              children: [
                TableCell.create({
                  children: [
                    Paragraph.create("Tableau récapitulatif des entrées de l'étape de calcul"),
                  ],
                }),
              ],
            }),
          ]->Array.concat(
            inputs
            ->Array.filter(({value}) => Utils.loggedValueIsEmbeddable(value))
            ->Array.sort((a, b) => Utils.loggedValueCompare(a.value, b.value))
            ->Array.flatMap(varDef => {
              if varDef.value->isLitLoggedValue {
                varDef->litVarDefToTableRow(~maxDepth=maxInputsDepth)
              } else {
                varDef->varDefToTableRow(~maxDepth=maxInputsDepth)
              }
            }),
          ),
        })

        let inputParagraphs = [
          Paragraph.create'({
            heading: #Heading3,
            children: [
              TextRun.create("Entrées utilisées pour l'étape de calcul "),
            ]->Array.concat(linkToSection(id)),
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
            ]->Array.concat(linkToSection(id)),
          }),
        ]->Array.concat(outputs->Array.flatMap(varDefToFileChilds))
        let explanationsParagraphs = [
          Paragraph.create'({
            heading: #Heading3,
            children: [TextRun.create("Explication pour l'étape de calcul ")]->Array.concat(
              linkToSection(id),
            ),
          }),
        ]->Array.concat(
          explanations->Array.flatMap(expl =>
            switch expl {
            | Ref(id) => [
                Paragraph.create'({
                  children: [TextRun.create("Calcul de l'étape ")]->Array.concat(
                    linkToSection(id),
                  ),
                }),
              ]
            | Def(varDef) => varDefToFileChilds(varDef)
            }
          ),
        )
        if id == SectionId.root {
          []
        } else {
          [
            Paragraph.create'({
              heading: #Heading2,
              children: [bookmarkSection(id, title)],
              pageBreakBefore: true,
            }),
            Paragraph.create'({
              children: if parent != SectionId.root {
                [TextRun.create("Cette étape de calcul intervient dans l'étape ")]->Array.concat(
                  linkToSection(parent),
                )
              } else {
                []
              },
            }),
          ]
          ->Array.concat([inputTable])
          ->Array.concat(outputParagraphs)
          ->Array.concat(explanationsParagraphs)
        }
      }
    })
  }
}
