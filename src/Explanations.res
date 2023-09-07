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
and explanation = Def(var_def) | Ref({id: SectionId.t, title: string})

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

let getTitle = (sections: sectionMap, id: SectionId.t): string => {
  switch sections->Map.get(id) {
  | Some({title}) => title
  | None => // TODO: what should we do here?
    "Unknown section"
  }
}

let rec parseExplanations = (events: array<event>, ctx: parseCtx): array<explanation> => {
  let makeNewSection = (name, inputs, body, outputs) => {
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
    Ref({id, title})
  }

  events->Array.flatMap(event => {
    switch event {
    | VarComputation({fun_calls: Some(calls)} as varDef) =>
      calls
      ->List.reverse
      ->List.map(({fun_name, fun_inputs, body, output}) => {
        makeNewSection(fun_name, fun_inputs, body, [output])
      })
      ->List.toArray
      ->Array.concat([Def(varDef)])
    | VarComputation(varDef) => [Def(varDef)]
    | SubScopeCall({sname, inputs, sbody}) => {
        let outputs = sbody->List.toArray->getOutputs
        [makeNewSection(sname, inputs, sbody, outputs)]
      }
    | FunCall({fun_name, fun_inputs, body, output}) => [
        makeNewSection(fun_name, fun_inputs, body, [output]),
      ]
    }
  })
}

let parseRoot = (ctx: parseCtx, events: array<event>): section => {
  let explanations = events
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

  let linkToSection = (~textRunOptions: TextRun.options={}, id: SectionId.t, title: string): array<
    Docx.ParagraphChild.t,
  > => {
    let bookmarkId = `section-${id->Int.toString}`
    let run = (~style="", text) => TextRun.make'({...textRunOptions, style, text})

    [
      InternalHyperlink.make({
        anchor: bookmarkId,
        children: [run(~style="Hyperlink", title)],
      }),
      run(` (p. `),
      PageReference.make(bookmarkId),
      run(`)`),
    ]
  }

  let bookmarkSection = (id: SectionId.t, title: string): Docx.ParagraphChild.t => {
    Bookmark.make({
      id: `section-${id->Int.toString}`,
      children: [
        TextRun.make'({
          text: `Étape n°${id->Int.toString} : ${title}`,
        }),
      ],
    })
  }

  @raises(Error.t)
  let outputToFileChilds = (~selectedOutput: information, explanationSectionMap: sectionMap): array<
    FileChild.t,
  > => {
    open FileChild

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
    let output = outputs->Array.find(({name}) => name == selectedOutput)
    let stepParagraphs = refs->Array.mapWithIndex((expl, i) => {
      switch expl {
      | Ref({id, title}) =>
        p'({
          numbering: {level: 0, reference: "decimal", instance: i->Int.toFloat},
          children: /* [TextRun.make(`${(i + 1)->Int.toString}. `)]->Array.concat( */
          linkToSection(id, title),
          // ),
        })
      | _ => p("")
      }
    })
    [
      p'({
        children: output
        ->Option.map(output => [
          TextRun.make(`La valeur calculée par le programme est `),
          TextRun.make'({
            text: output.name->List.reverse->List.headExn,
            style: "VariableName",
          }),
          TextRun.make(` et vaut `),
          if output.value->Utils.isLitLoggedValue {
            output.value->TableUtils.litLoggedValueToParagraphChild
          } else {
            TextRun.make("TODO (non-literal value)")
          },
          TextRun.make("."),
        ])
        ->Option.getWithDefault([]),
      }),
      p(""),
      p(`La valeur a été calculée à partir des étapes de calculs : `),
    ]->Array.concat(stepParagraphs)
  }

  let getTableWithLinkToSection = (
    ~id,
    ~title,
    ~headingText,
    ~bgColor,
    ~maxDepth,
    ~contentRows,
  ) => {
    let textHeadingStyle: TextRun.options = {bold: true, size: "10pt"}
    let headingParagraph = Paragraph.make'({
      spacing: {before: 80.0, after: 80.0},
      children: [
        TextRun.make'({
          ...textHeadingStyle,
          text: headingText ++ " ",
        }),
      ]->Array.concat(linkToSection(~textRunOptions=textHeadingStyle, id, title)),
    })

    TableUtils.getTable(~bgColor, ~maxDepth, ~contentRows, ~headingParagraph)
  }

  let getInputsTable = (id: SectionId.t, title: string, inputs: array<var_def>) => {
    let headingText = "Entrées de l'étape de calcul"
    let maxDepth = inputs->Utils.getMaxDepth
    let bgColor = #blue_france_925
    let contentRows = inputs->TableUtils.getTableRows(~bgColorRef=ref(bgColor), ~maxDepth)

    getTableWithLinkToSection(~id, ~title, ~headingText, ~bgColor, ~maxDepth, ~contentRows)
  }

  let getOutputsTable = (id: SectionId.t, title: string, outputs: array<var_def>) => {
    let headingText = "Résultats de l'étape de calcul"
    let maxDepth = outputs->Utils.getMaxDepth
    let bgColor = #red_marianne_925
    let contentRows = outputs->TableUtils.getTableRows(~bgColorRef=ref(bgColor), ~maxDepth)

    getTableWithLinkToSection(~id, ~title, ~headingText, ~bgColor, ~maxDepth, ~contentRows)
  }

  let getExplanationsTable = (id: SectionId.t, title: string, explanations: array<explanation>) => {
    let headingText = "Détails de l'étape de calcul"
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
      | Ref({id, title}) => [
          TableRow.make({
            children: [
              TableUtils.getNormalTableCell(
                ~bgColor=bgColorRef.contents,
                [
                  TableUtils.getNormalTableCellParagraph(
                    [TextRun.make("Calcul de l'étape ")]->Array.concat(linkToSection(id, title)),
                  ),
                ],
              ),
            ],
          }),
        ]
      }
    })

    getTableWithLinkToSection(~id, ~title, ~headingText, ~bgColor, ~maxDepth, ~contentRows)
  }

  let explanationsToFileChilds = (explanationSectionMap: sectionMap): array<FileChild.t> => {
    explanationSectionMap
    ->Map.entries
    ->Iterator.toArray
    ->Array.sort(((id, _), (id', _)) => SectionId.compare(id, id'))
    ->Array.flatMap(((id, {title, inputs, outputs, explanations, parent})) => {
      if id == SectionId.root {
        []
      } else {
        [
          FileChild.p'({
            heading: #Heading2,
            children: [bookmarkSection(id, title)],
            pageBreakBefore: true,
          }),
          FileChild.p'({
            children: if parent != SectionId.root {
              let parentTitle = explanationSectionMap->getTitle(parent)
              [TextRun.make("Cette étape de calcul intervient dans l'étape ")]->Array.concat(
                linkToSection(parent, parentTitle),
              )
            } else {
              []
            },
          }),
        ]->Array.concat([
          FileChild.fromTable(getInputsTable(id, title, inputs)),
          FileChild.p(""),
          FileChild.fromTable(getOutputsTable(id, title, outputs)),
          FileChild.p(""),
          FileChild.fromTable(getExplanationsTable(id, title, explanations)),
        ])
      }
    })
  }
}
