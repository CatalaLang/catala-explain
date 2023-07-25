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
          spacing: {before: 40, after: 40},
        }),
      ],
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
        width: {size: 4.0, _type: #pct},
        borders: {
          bottom: {
            style: #none,
          },
        },
      }),
    )
  }

  let litVarDefToTableRow = (~depth=0, ~maxDepth, {name, value, pos}: var_def): array<
    TableRow.t,
  > => {
    let varNameCell = TableCell.create({
      children: [
        Paragraph.create'({
          children: [TextRun.create'({text: name->Utils.lastExn, style: "NormalTableCellText"})],
          spacing: {before: 40, after: 40},
        }),
      ],
      columnSpan: maxDepth - depth,
      borders: {
        bottom: {
          style: #none,
        },
      },
    })

    let varDef = TableRow.create({
      children: depth->getEmptyCellArray->Array.concat([varNameCell, value->litLogValueToCell]),
    })

    switch getLawHeadingInTableRow(pos) {
    | Some(breadcrumpsRow) => [varDef, breadcrumpsRow]
    | None => [varDef]
    }
  }

  // TODO: could be factorized
  let rec varDefToTableRow = (~depth=0, ~maxDepth, {name, value, pos}: var_def): array<
    TableRow.t,
  > => {
    switch value {
    | Enum(_, (_, val)) => {
        // Unwrap the enum
        let varDefWithoutInfos = Utils.getVarDefWithoutInfos(name, val)
        if val->isLitLoggedValue {
          litVarDefToTableRow(~maxDepth, ~depth, varDefWithoutInfos)
        } else {
          varDefToTableRow(~maxDepth, ~depth, varDefWithoutInfos)
        }
      }
    | Struct(structName, fields) => {
        let varNameRow = TableRow.create({
          children: depth
          ->getEmptyCellArray
          ->Array.concat([
            TableCell.create({
              children: [
                Paragraph.create'({
                  children: [
                    TextRun.create'({text: name->Utils.lastExn, style: "BoldTableCellText"}),
                    TextRun.create'({
                      text: ` (de type ${structName->List.toArray->Array.joinWith(".")})`,
                      style: "ItalicTableCellText",
                    }),
                  ],
                  spacing: {before: 40, after: 40},
                }),
              ],
              columnSpan: maxDepth - depth + 1,
              borders: {
                bottom: {
                  style: #none,
                },
              },
            }),
          ]),
        })

        let valueRows =
          fields
          ->List.toArray
          ->Array.filter(((_, value)) => Utils.loggedValueIsEmbeddable(value))
          ->Array.sort(((_, v1), (_, v2)) => Utils.loggedValueCompare(v1, v2))
          ->Array.flatMap(((field, value)) => {
            let varDefWithoutInfos = Utils.getVarDefWithoutInfos(list{field}, value)
            if value->isLitLoggedValue {
              litVarDefToTableRow(~maxDepth, ~depth=depth + 1, varDefWithoutInfos)
            } else {
              varDefToTableRow(~maxDepth, ~depth=depth + 1, varDefWithoutInfos)
            }
          })

        let emptyRow = TableRow.create({
          children: [
            TableCell.create({
              children: [],
              columnSpan: maxDepth,
            }),
          ],
        })

        switch getLawHeadingInTableRow(pos) {
        | Some(breadcrumpsRow) => [emptyRow, varNameRow, breadcrumpsRow]
        | None => [emptyRow, varNameRow]
        }->Array.concat(valueRows)
      }
    | Array(l) => {
        let elemNb = ref(0)
        l->Array.flatMap(value => {
          elemNb := elemNb.contents + 1
          let varDefWithoutInfos = Utils.getVarDefWithoutInfos(
            name->List.map(n => n ++ " (élément " ++ elemNb.contents->Int.toString ++ ")"),
            value,
          )
          if value->isLitLoggedValue {
            litVarDefToTableRow(~maxDepth, ~depth, varDefWithoutInfos)
          } else {
            varDefToTableRow(~maxDepth, ~depth, varDefWithoutInfos)
          }
        })
      }
    | _ => Js.Exn.raiseError(`Non-literal value is expected.`)
    }
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
    let rec loggedValueGetMaxDepth = (~depth=1, value: LoggedValue.t): int => {
      switch value {
      | Struct(_, l) =>
        l
        ->List.toArray
        ->Array.map(((_, value)) => value->loggedValueGetMaxDepth(~depth=depth + 1))
        ->Array.reduce(1, (a, b) => Math.Int.max(a, b))
      | Enum(_, (_, l)) if l->isLitLoggedValue => depth
      | Enum(_, (_, l)) =>
        // NOTE(@EmileRolley): we need to unwrap the enum first, because we
        // don't want to count the enum itself
        l->loggedValueGetMaxDepth(~depth)
      | Array(elems) =>
        elems
        ->Array.map(value => value->loggedValueGetMaxDepth(~depth))
        ->Array.reduce(1, (a, b) => Math.Int.max(a, b))
      | _ => depth
      }
    }

    inputs
    ->Array.map(({value}) => {
      if !(value->isLitLoggedValue) {
        value->loggedValueGetMaxDepth
      } else {
        1
      }
    })
    ->Array.reduce(1, (a, b) => Math.Int.max(a, b))
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
        let columnWidths = Array.make(~length=maxInputsDepth - 1, 4.0)->Array.concat([65.0, 25.0])
        let inputTable = Table.create({
          columnWidths,
          width: {size: 100.0, _type: #pct},
          layout: #fixed,
          alignment: #center,
          rows: [
            TableRow.create({
              tableHeader: true,
              children: [
                TableCell.create({
                  children: [
                    Paragraph.create'({
                      children: [
                        TextRun.create("Entrées utilisées pour l'étape de calcul"),
                      ]->Array.concat(id->linkToSection),
                      // style: "TableHeadingText",
                    }),
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
