open CatalaRuntime
open Styles
open DsfrColors

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

  let linkToSection = (~textRunOptions: TextRun.options={}, id: SectionId.t): array<
    Docx.paragraph_child,
  > => {
    let bookmarkId = `section-${id->Int.toString}`
    let run = (~style="", text) => TextRun.create'({...textRunOptions, style, text})

    [
      InternalHyperlink.create({
        anchor: bookmarkId,
        children: [run(~style="Hyperlink", `n°${id->Int.toString}`)],
      }),
      run(` (p. `),
      PageReference.create(bookmarkId),
      run(`)`),
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

  let getLawHeadingInTableRow = (
    ~bgColor: DsfrColors.t,
    pos: option<CatalaRuntime.sourcePosition>,
  ): option<TableRow.t> => {
    switch pos {
    | Some(pos) =>
      TableRow.create({
        children: [
          TableCell.create({
            children: [
              Utils.getNormalTableCellParagraph([Utils.getLawHeadingBreadcrumbsLink(pos)]),
            ],
            width: {size: 100.0, _type: #pct},
            shading: {
              fill: bgColor->toHex,
            },
            borders: {
              top: {
                style: #dotted,
                size: 0.25,
                color: #grey_main_525->toHex,
              },
            },
          }),
        ],
      })->Some
    | None => None
    }
  }

  let litLogValueToCell = (
    ~bgColor: DsfrColors.t,
    ~borders: TableCell.table_cell_borders_options={},
    val: LoggedValue.t,
  ): TableCell.t => {
    Utils.getNormalTableCell(
      ~bgColor,
      ~borders,
      [Utils.getNormalTableCellParagraph(~alignment=#right, [val->litLoggedValueToParagraphChild])],
    )
  }

  let getEmptyCellArray = (~bgColor: DsfrColors.t, length: int): array<TableCell.t> => {
    Array.make(~length, 0)->Array.mapWithIndex((_, idx) => {
      TableCell.create({
        children: [],
        width: {size: 4.0, _type: #pct},
        shading: {fill: bgColor->toHex},
        borders: idx != 0
          ? {
              left: {
                style: #dotted,
                color: "#808080",
                size: 5.0,
              },
              right: {style: #none},
            }
          : {
              right: {style: #none},
            },
      })
    })
  }

  let litVarDefToTableRow = (
    ~depth=0,
    ~maxDepth,
    ~bgColor: DsfrColors.t,
    {name, value, pos}: var_def,
  ): array<TableRow.t> => {
    let lawHeadingOpt = getLawHeadingInTableRow(~bgColor, pos)
    let borders: TableCell.table_cell_borders_options =
      lawHeadingOpt->Option.isSome ? {bottom: {style: #none}} : {}
    let varNameCell = Utils.getNormalTextTableCell(
      ~columnSpan=maxDepth - depth,
      ~borders,
      ~bgColor,
      name->Utils.lastExn,
    )
    let varDef = TableRow.create({
      children: depth
      ->getEmptyCellArray(~bgColor)
      ->Array.concat([varNameCell, value->litLogValueToCell(~bgColor, ~borders)]),
    })

    switch lawHeadingOpt {
    | Some(breadcrumpsRow) => [varDef, breadcrumpsRow]
    | None => [varDef]
    }
  }

  // TODO: could be factorized
  let rec varDefToTableRow = (
    ~depth=0,
    ~maxDepth,
    ~bgColorRef: ref<DsfrColors.t>,
    {name, value, pos}: var_def,
  ): array<TableRow.t> => {
    switch value {
    | Enum(_, (_, val)) => {
        // Unwrap the enum
        let varDefWithoutInfos = Utils.getVarDefWithoutInfos(name, val)
        if val->Utils.isLitLoggedValue {
          litVarDefToTableRow(~maxDepth, ~depth, ~bgColor=bgColorRef.contents, varDefWithoutInfos)
        } else {
          varDefToTableRow(~maxDepth, ~depth, ~bgColorRef, varDefWithoutInfos)
        }
      }
    | Struct(structName, fields) => {
        let bgColor = bgColorRef.contents

        let varNameRow = TableRow.create({
          children: depth
          ->getEmptyCellArray(~bgColor)
          ->Array.concat([
            Utils.getNormalTableCell(
              ~columnSpan=maxDepth - depth + 1,
              ~borders={bottom: {style: #none}},
              ~bgColor,
              [
                Utils.getNormalTableCellParagraph([
                  TextRun.create'({text: name->Utils.lastExn, bold: true}),
                  TextRun.create'({
                    text: ` (de type ${structName->List.toArray->Array.joinWith(".")})`,
                    italics: true,
                    bold: true,
                  }),
                ]),
              ],
            ),
          ]),
        })

        let valueRows =
          fields
          ->List.toArray
          ->Array.filter(((_, value)) => Utils.loggedValueIsEmbeddable(value))
          ->Array.sort(((_, v1), (_, v2)) => Utils.loggedValueCompare(v1, v2))
          ->Array.flatMap(((field, value)) => {
            let varDefWithoutInfos = Utils.getVarDefWithoutInfos(list{field}, value)

            bgColorRef := bgColorRef.contents->DsfrColors.getNextRowColor
            if value->Utils.isLitLoggedValue {
              litVarDefToTableRow(
                ~maxDepth,
                ~depth=depth + 1,
                ~bgColor=bgColorRef.contents,
                varDefWithoutInfos,
              )
            } else {
              varDefToTableRow(~maxDepth, ~depth=depth + 1, ~bgColorRef, varDefWithoutInfos)
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

        switch getLawHeadingInTableRow(~bgColor, pos) {
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

          bgColorRef := bgColorRef.contents->DsfrColors.getNextRowColor
          if value->Utils.isLitLoggedValue {
            litVarDefToTableRow(~maxDepth, ~depth, ~bgColor=bgColorRef.contents, varDefWithoutInfos)
          } else {
            varDefToTableRow(~maxDepth, ~depth, ~bgColorRef, varDefWithoutInfos)
          }
        })
      }
    | _ => Js.Exn.raiseError(`Non-literal value is expected.`)
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
          if output.value->Utils.isLitLoggedValue {
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

  let getTableRows = (~bgColorRef: ref<DsfrColors.t>, ~maxDepth, defs: array<var_def>): array<
    TableRow.t,
  > => {
    defs
    ->Array.filter(({value}) => Utils.loggedValueIsEmbeddable(value))
    ->Array.sort((a, b) => Utils.loggedValueCompare(a.value, b.value))
    ->Array.flatMap(varDef => {
      bgColorRef := bgColorRef.contents->getNextRowColor
      if varDef.value->Utils.isLitLoggedValue {
        varDef->litVarDefToTableRow(~maxDepth, ~bgColor=bgColorRef.contents)
      } else {
        varDef->varDefToTableRow(~maxDepth, ~bgColorRef)
      }
    })
  }

  let getTable = (
    ~id: SectionId.t,
    ~headingText: string,
    ~maxDepth: int,
    ~bgColorRef: ref<DsfrColors.t>,
    ~contentRows: array<TableRow.t>,
  ): file_child => {
    let textHeadingStyle: TextRun.options = {bold: true, size: "10pt"}
    let innerBorder = {style: #single, color: #grey_main_525->toHex, size: 0.25}

    Table.create({
      columnWidths: Array.make(~length=maxDepth - 1, 4.0)->Array.concat([65.0, 25.0]),
      width: {size: 100.0, _type: #pct},
      alignment: #center,
      borders: {
        bottom: {style: #single},
        insideHorizontal: innerBorder,
        insideVertical: innerBorder,
      },
      rows: [
        TableRow.create({
          tableHeader: true,
          children: [
            TableCell.create({
              shading: {fill: bgColorRef.contents->toHex},
              children: [
                Paragraph.create'({
                  spacing: {before: 80, after: 80},
                  children: [
                    TextRun.create'({
                      ...textHeadingStyle,
                      text: headingText ++ " ",
                    }),
                  ]->Array.concat(id->linkToSection(~textRunOptions=textHeadingStyle)),
                }),
              ],
            }),
          ],
        }),
      ]->Array.concat(contentRows),
    })
  }

  let explanationsToFileChilds = (explanationSectionMap: sectionMap): array<file_child> => {
    explanationSectionMap
    ->Map.entries
    ->Iterator.toArray
    ->Array.sort(((id, _), (id', _)) => SectionId.compare(id, id'))
    ->Array.flatMap(((id, {title, inputs, outputs, explanations, parent})) => {
      let maxInputsDepth = inputs->Utils.getMaxDepth
      let inputBgColorRef = ref(#blue_france_925)
      let inputsTable = getTable(
        ~id,
        ~headingText="Entrées utilisées pour l'étape de calcul",
        ~bgColorRef=inputBgColorRef,
        ~maxDepth=maxInputsDepth,
        ~contentRows=inputs->getTableRows(~bgColorRef=inputBgColorRef, ~maxDepth=maxInputsDepth),
      )

      let maxOutputDepth = outputs->Utils.getMaxDepth
      let outputBgColorRef = ref(#red_marianne_925)
      let outputsTable = getTable(
        ~id,
        ~headingText=outputs->Array.length > 1
          ? "Valeurs calculées dans l'étape de calcul"
          : "Valeur calculée dans l'étape de calcul",
        ~bgColorRef=outputBgColorRef,
        ~maxDepth=maxOutputDepth,
        ~contentRows=outputs->getTableRows(~bgColorRef=outputBgColorRef, ~maxDepth=maxOutputDepth),
      )

      let explanationMaxDepth =
        explanations
        ->Array.filterMap((expl: explanation) =>
          switch expl {
          | Def(varDef) => Some(varDef)
          | _ => None
          }
        )
        ->Utils.getMaxDepth
      let explanationBgColorRef = ref(#green_emeraude_925)
      let explanationsTable = getTable(
        ~id,
        ~headingText="Explication pour l'étape de calcul",
        ~bgColorRef=explanationBgColorRef,
        ~maxDepth=explanationMaxDepth,
        ~contentRows=explanations->Array.flatMap(expl => {
          explanationBgColorRef := explanationBgColorRef.contents->getNextRowColor
          switch expl {
          | Def(varDef) =>
            if varDef.value->Utils.isLitLoggedValue {
              varDef->litVarDefToTableRow(
                ~maxDepth=explanationMaxDepth,
                ~bgColor=explanationBgColorRef.contents,
              )
            } else {
              varDef->varDefToTableRow(
                ~maxDepth=explanationMaxDepth,
                ~bgColorRef=explanationBgColorRef,
              )
            }
          | Ref(id) => [
              TableRow.create({
                children: [
                  Utils.getNormalTableCell(
                    ~bgColor=explanationBgColorRef.contents,
                    [
                      Utils.getNormalTableCellParagraph(
                        [TextRun.create("Calcul de l'étape ")]->Array.concat(linkToSection(id)),
                      ),
                    ],
                  ),
                ],
              }),
            ]
          }
        }),
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
        ]->Array.concat([
          inputsTable,
          Paragraph.create(""),
          outputsTable,
          Paragraph.create(""),
          explanationsTable,
        ])
      }
    })
  }
}
