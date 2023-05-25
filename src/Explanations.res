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
  output: option<var_def>,
  explanations: array<explanation>,
}
and explanation = Def(var_def) | Ref(SectionId.t)

type sectionMap = Map.t<SectionId.t, section>

let getOutputExn = (event: event): var_def => {
  switch event {
  | VarComputation(var_def) => var_def
  | _ => Exn.raiseError("Last event is expected to be a variable definition ouput")
  }
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
        let output = explanations->Array.shift->Option.map(getOutputExn)
        let section = {
          id,
          parent: ctx.currentId,
          title,
          inputs: List.toArray(inputs),
          output,
          explanations: explanations->parseExplanations({...ctx, currentId: id}),
        }
        ctx.sections->Map.set(id, section)
        Console.log2(`SubScopeCall ${id->Int.toString}`, ctx.sections)
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
  // A program should always have an output
  let output = explanations->Array.shift->Option.map(getOutputExn)
  {
    id: SectionId.root,
    title: "TODO",
    inputs: [],
    output,
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
  let loggedValueToParagraphChild = (val: LoggedValue.t): paragraphChild => {
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
    | _ =>
      TextRun.create'({
        text: `TODO: ${val->LoggedValue.loggedValueToString(1)}`,
        style: "EnumLiteral",
      })
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
    let {output, explanations} =
      explanationSectionMap
      ->Map.get(SectionId.root)
      ->Utils.getJsErr("Root section not found in [explanationSectionMap]")
    [
      Paragraph.create'({
        children: output
        ->Option.map(output => [
          TextRun.create(`La valeur calculée par le programme est `),
          TextRun.create'({
            text: output.name->List.reverse->List.headExn,
            style: "VariableName",
          }),
          // output.name->informationToParagraphChilds,
          TextRun.create(` et vaut `),
          output.value->loggedValueToParagraphChild,
          TextRun.create("."),
        ])
        ->Option.getWithDefault([]),
      }),
      Paragraph.create'({
        children: [TextRun.create(`La valeur a été calculée à partir de : `)]->Array.concat(
          explanations->Array.map(expl =>
            switch expl {
            | Ref(id) => {
                let {title} =
                  explanationSectionMap
                  ->Map.get(id)
                  ->Utils.getJsErr(
                    `Section ${id->Int.toString} not found in [explanationSectionMap]`,
                  )
                InternalHyperlink.create({
                  anchor: `section-${id->Int.toString}`,
                  children: [
                    TextRun.create'({
                      text: title,
                      style: "Hyperlink",
                    }),
                  ],
                })
              }
            | _ => TextRun.create("")
            }
          ),
        ),
      }),
    ]
  }

  let explanationsToFileChilds = (explanationSectionMap: sectionMap): array<fileChild> => {
    explanationSectionMap
    ->Map.entries
    ->Iterator.toArray
    ->Array.flatMap(((id, {title, inputs, output, explanations})) => {
      [
        Paragraph.create'({
          heading: #Heading2,
          children: [
            Bookmark.create({
              id: `section-${id->Int.toString}`,
              children: [
                TextRun.create'({
                  text: title,
                }),
              ],
            }),
          ],
        }),
        // Paragraph.create'({
        //   text: "Entrées",
        //   heading: #Heading3,
        // }),
        // Paragraph.create'({
        //   children: inputs->Array.flatMap(input => [
        //     TextRun.create'({
        //       text: input.name->List.reverse->List.headExn,
        //       style: "VariableName",
        //     }),
        //     TextRun.create(` : `),
        //     input.value->loggedValueToParagraphChild,
        //   ]),
        // }),
        // Paragraph.create'({
        //   text: "Sortie",
        //   heading: #Heading3,
        // }),
        // Paragraph.create'({
        //   children: output
        //   ->Option.map(output => [
        //     TextRun.create'({
        //       text: output.name->List.reverse->List.headExn,
        //       style: "VariableName",
        //     }),
        //     TextRun.create(` : `),
        //     output.value->loggedValueToParagraphChild,
        //   ])
        //   ->Option.getWithDefault([]),
        // }),
        // Paragraph.create'({
        //   text: "Explications",
        //   heading: #Heading3,
        // }),
        // Paragraph.create'({
        //   children: explanations->Array.map(expl =>
        //     switch expl {
        //     | Ref(id) => {
        //         let section =
        //           explanationSectionMap
        //           ->Map.get(id)
        //           ->Utils.getJsErr(
        //             `Section ${id->Int.toString} not found in [explanationSectionMap]`,
        //           )
        //         InternalHyperlink.create({
        //           anchor: `section-${id->Int.toString}`,
        //           children: [
        //             TextRun.create'({
        //               text: `${section.title} - ${id->Int.toString}`,
        //               style: "VariableName",
        //             }),
        //           ],
        //         })
        //       }
        //     | _ => TextRun.create("")
        //     }
        //   ),
        // }),
      ]
    })
  }
}
