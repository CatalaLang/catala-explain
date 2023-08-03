open CatalaRuntime
open Styles
open DSFRColors

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
            output.value->TableUtils.litLoggedValueToParagraphChild
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

  let getTable = (
    ~id: SectionId.t,
    ~headingText: string,
    ~maxDepth: int,
    ~bgColor: DSFRColors.t,
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
              shading: {fill: bgColor->toHex},
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

  let getInputsTable = (id: SectionId.t, inputs: array<var_def>) => {
    let headingText =
      inputs->Array.length > 1
        ? "Entrées utilisées pour l'étape de calcul"
        : "Entrée utilisée pour l'étape de calcul"
    let maxDepth = inputs->Utils.getMaxDepth
    let bgColor = #blue_france_925
    let contentRows = inputs->TableUtils.getTableRows(~bgColorRef=ref(bgColor), ~maxDepth)

    getTable(~id, ~headingText, ~bgColor, ~maxDepth, ~contentRows)
  }

  let getOutputsTable = (id: SectionId.t, outputs: array<var_def>) => {
    let headingText =
      outputs->Array.length > 1
        ? "Valeurs calculées dans l'étape de calcul"
        : "Valeur calculée dans l'étape de calcul"
    let maxDepth = outputs->Utils.getMaxDepth
    let bgColor = #red_marianne_925
    let contentRows = outputs->TableUtils.getTableRows(~bgColorRef=ref(bgColor), ~maxDepth)

    getTable(~id, ~headingText, ~bgColor, ~maxDepth, ~contentRows)
  }

  let getExplanationsTable = (id: SectionId.t, explanations: array<explanation>) => {
    let headingText =
      explanations->Array.length > 1
        ? "Explications pour l'étape de calcul"
        : "Explication pour l'étape de calcul"
    let maxDepth =
      explanations
      ->Array.filterMap((expl: explanation) =>
        switch expl {
        | Def(varDef) => Some(varDef)
        | _ => None
        }
      )
      ->Utils.getMaxDepth
    let bgColor = #green_emeraude_925
    let bgColorRef = ref(bgColor)
    let contentRows = explanations->Array.flatMap(expl => {
      bgColorRef := bgColorRef.contents->getNextRowColor
      switch expl {
      | Def(varDef) =>
        if varDef.value->Utils.isLitLoggedValue {
          varDef->TableUtils.litVarDefToTableRow(~maxDepth, ~bgColor=bgColorRef.contents)
        } else {
          varDef->TableUtils.varDefToTableRow(~maxDepth, ~bgColorRef)
        }
      | Ref(id) => [
          TableRow.create({
            children: [
              TableUtils.getNormalTableCell(
                ~bgColor=bgColorRef.contents,
                [
                  TableUtils.getNormalTableCellParagraph(
                    [TextRun.create("Calcul de l'étape ")]->Array.concat(linkToSection(id)),
                  ),
                ],
              ),
            ],
          }),
        ]
      }
    })

    getTable(~id, ~headingText, ~bgColor, ~maxDepth, ~contentRows)
  }

  let explanationsToFileChilds = (explanationSectionMap: sectionMap): array<file_child> => {
    explanationSectionMap
    ->Map.entries
    ->Iterator.toArray
    ->Array.sort(((id, _), (id', _)) => SectionId.compare(id, id'))
    ->Array.flatMap(((id, {title, inputs, outputs, explanations, parent})) => {
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
          getInputsTable(id, inputs),
          Paragraph.create(""),
          getOutputsTable(id, outputs),
          Paragraph.create(""),
          getExplanationsTable(id, explanations),
        ])
      }
    })
  }
}
