open CatalaRuntime
open Styles
open DSFRColors

module SectionId = UId.Int.Make()

/**
  A section is a step of the computation, with a title, a list of inputs and
  outputs, and a list of explanations.

  Each sub-scope call or function call is considered a section. The top-level
  scope is also a section (with the [id == SectionId.root]).

  An explanation is either a definition of a variable, or a reference to another
  section.
*/
type rec section = {
  id: SectionId.t,
  parent: SectionId.t,
  scopeName: information,
  inputs: array<var_def>,
  outputs: array<var_def>,
  explanations: array<explanation>,
}
and explanation = Def(var_def) | Ref({id: SectionId.t, scopeName: information})

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

let getScopeName = (sections: sectionMap, id: SectionId.t): information => {
  switch sections->Map.get(id) {
  | Some({scopeName}) => scopeName
  | None => list{}
  }
}

let rec parseExplanations = (events: array<event>, ctx: parseCtx): array<explanation> => {
  let makeNewSection = (name, inputs, body, outputs) => {
    let id = SectionId.fresh()
    let outputExplanations =
      // Parse explanations of nested function calls
      outputs
      ->Array.filterMap(({fun_calls}) =>
        fun_calls->Option.flatMap(def => Some(def->List.toArray->Array.map(def => FunCall(def))))
      )
      ->Array.flat
      ->parseExplanations({...ctx, currentId: id})

    let section = {
      id,
      parent: ctx.currentId,
      scopeName: name,
      inputs: List.toArray(inputs),
      outputs,
      explanations: body
      ->List.toArray
      ->parseExplanations({...ctx, currentId: id})
      ->Array.concat(outputExplanations),
    }
    ctx.sections->Map.set(id, section)
    Ref({id, scopeName: name})
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
    scopeName: firstOutput.name,
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

  let linkToSection = (~size: option<int>=None, id: SectionId.t, scopeName: information): array<
    Docx.ParagraphChild.t,
  > => {
    let bookmarkId = Utils.getBookmarkId(id)
    let run = text =>
      switch size {
      | Some(n) => TextRun.make'({text, size: `${n->Int.toString}pt`})
      | None => TextRun.make(text)
      }

    [
      InternalHyperlink.make({
        anchor: bookmarkId,
        children: scopeName->Utils.getSectionTitle(~size),
      }),
      run(` (p. `),
      PageReference.make(bookmarkId),
      run(`)`),
    ]
  }

  let bookmarkSection = (id: SectionId.t, scopeName: information): Docx.ParagraphChild.t => {
    Bookmark.make({
      id: `section-${id->Int.toString}`,
      children: [
        TextRun.make'({
          text: `Étape n°${id->Int.toString} : `,
        }),
      ]->Array.concat(scopeName->Utils.getSectionTitle(~size=Some(18))),
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
      | Ref({id, scopeName}) =>
        p'({
          numbering: {level: 0, reference: "decimal", instance: i->Int.toFloat},
          children: linkToSection(id, scopeName, ~size=Some(8)),
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
    ~scopeName,
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
      ]->Array.concat(linkToSection(~size=Some(10), id, scopeName)),
    })

    TableUtils.getTable(~bgColor, ~maxDepth, ~contentRows, ~headingParagraph)
  }

  let getInputsTable = (id: SectionId.t, scopeName: information, inputs: array<var_def>) => {
    let headingText = "Entrées de l'étape de calcul"
    let maxDepth = inputs->Utils.getMaxDepth
    let bgColor = #blue_france_925
    let contentRows = inputs->TableUtils.getTableRows(~bgColorRef=ref(bgColor), ~maxDepth)

    getTableWithLinkToSection(~id, ~scopeName, ~headingText, ~bgColor, ~maxDepth, ~contentRows)
  }

  let getOutputsTable = (id: SectionId.t, scopeName: information, outputs: array<var_def>) => {
    let headingText = "Résultats de l'étape de calcul"
    let maxDepth = outputs->Utils.getMaxDepth
    let bgColor = #red_marianne_925
    let contentRows = outputs->TableUtils.getTableRows(~bgColorRef=ref(bgColor), ~maxDepth)

    getTableWithLinkToSection(~id, ~scopeName, ~headingText, ~bgColor, ~maxDepth, ~contentRows)
  }

  let getExplanationsTable = (
    id: SectionId.t,
    scopeName: information,
    explanations: array<explanation>,
  ) => {
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
      | Ref({id, scopeName}) => [
          TableRow.make({
            children: [
              TableUtils.getNormalTableCell(
                ~bgColor=bgColorRef.contents,
                [
                  TableUtils.getNormalTableCellParagraph(
                    [TextRun.make("Calcul de l'étape ")]->Array.concat(
                      linkToSection(id, scopeName),
                    ),
                  ),
                ],
              ),
            ],
          }),
        ]
      }
    })

    getTableWithLinkToSection(~id, ~scopeName, ~headingText, ~bgColor, ~maxDepth, ~contentRows)
  }

  let getIntermediateTOC = (text, explanations: array<explanation>): array<FileChild.t> => {
    [FileChild.p(text)]->Array.concat(
      explanations->Array.filterMap(expl => {
        switch expl {
        | Ref({id, scopeName}) =>
          Some(
            FileChild.p'({
              children: linkToSection(id, scopeName, ~size=Some(8)),
              numbering: {level: 0, reference: "bullet"},
            }),
          )
        | _ => None
        }
      }),
    )
  }

  let explanationsToFileChilds = (explanationSectionMap: sectionMap): array<FileChild.t> => {
    explanationSectionMap
    ->Map.entries
    ->Iterator.toArray
    ->Array.sort(((id, _), (id', _)) => SectionId.compare(id, id'))
    ->Array.flatMap(((id, {scopeName, inputs, outputs, explanations, parent})) => {
      if id == SectionId.root {
        []
      } else {
        let parentTitle = explanationSectionMap->getScopeName(parent)
        [
          FileChild.p'({
            heading: #Heading3,
            children: [bookmarkSection(id, scopeName)],
            pageBreakBefore: id != 1,
          }),
        ]->Array.concatMany([
          parent != SectionId.root
            ? getIntermediateTOC(
                "Cette étape intervient dans l'étape calcul :",
                [Ref({id: parent, scopeName: parentTitle})],
              )
            : [],
          explanations->Array.length != 0
            ? getIntermediateTOC(
                "Cette étape dépend des étapes de calculs suivantes :",
                explanations,
              )
            : [],
          [
            FileChild.p(""),
            FileChild.fromTable(getInputsTable(id, scopeName, inputs)),
            FileChild.p(""),
            FileChild.fromTable(getOutputsTable(id, scopeName, outputs)),
            FileChild.p(""),
            FileChild.fromTable(getExplanationsTable(id, scopeName, explanations)),
          ],
        ])
      }
    })
  }
}
