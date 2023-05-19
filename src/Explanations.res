open CatalaRuntime

module Id = {
  type t = int

  let root = -1

  let id = ref(0)

  let fresh = () => {
    id := id.contents + 1
    id.contents
  }
}

type rec section = {
  id: Id.t,
  parent?: Id.t,
  title: string,
  inputs: array<var_def>,
  output: var_def,
  explanations: array<explanation>,
}
and explanation = Def(var_def) | Ref(Id.t)

type sectionMap = Map.t<Id.t, section>

let getOutputExn = (event: event): var_def => {
  switch event {
  | VarComputation(var_def) => var_def
  | _ => Exn.raiseError("Last event is expected to be a variable definition ouput")
  }
}

let parseSection = (events: array<event>): section => {
  let explanations = events->Array.reverse
  let output = explanations->Array.shift->Option.getExn->getOutputExn
  {
    id: Id.root,
    title: "TODO",
    inputs: [],
    output,
    explanations: [],
  }
}

let fromEvents = (events: array<event>): sectionMap => {
  let sections = Map.make()
  let root = parseSection(events)
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
    | _ => failwith("TODO")
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

  let outputToFileChilds = (explanationSectionMap: sectionMap): array<fileChild> => {
    let {output} = explanationSectionMap->Map.get(Id.root)->Option.getExn
    [
      Paragraph.create'({text: "Résultats du programme", heading: HeadingLevel.h1}),
      Paragraph.create'({
        children: [
          TextRun.create(`La valeur calculée par le programme est `),
          TextRun.create'({
            text: output.name->List.reverse->List.headExn,
            style: "VariableName",
          }),
          // output.name->informationToParagraphChilds,
          TextRun.create(` et vaut `),
          output.value->loggedValueToParagraphChild,
          TextRun.create("."),
        ],
      }),
    ]
  }
}
