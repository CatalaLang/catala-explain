open Docx
open Promise

let rec eventToFileChild = (event: CatalaRuntime.event) => {
  open CatalaRuntime

  switch event {
  | SubScopeCall({sname, sbody}) =>
    [Paragraph.create("SubScopeCall: " ++ sname->List.toArray->Array.joinWith(" "))]->Array.concat(
      sbody->List.toArray->Array.flatMap(eventToFileChild),
    )
  | _ => []
  }
}

let getUserInputDocSection = (~userInputs: JSON.t, ~jsonSchema: JSON.t): Document.section => {
  {
    children: [
      Paragraph.create'({text: "Entrées du programme", heading: #Heading1}),
    ]->Array.concat(
      UserInputs.fromJSON(~json=userInputs, ~schema=jsonSchema)->UserInputs.Docx.toFileChild,
    ),
  }
}

let getResultDocSection = (explanationSectionMap: Explanations.sectionMap): Document.section => {
  {
    children: explanationSectionMap->Explanations.Docx.outputToFileChilds,
  }
}

type options = {
  title?: string,
  creator?: string,
  description?: string,
  filename: string,
  jsonSchema?: JSON.t,
}

let generate = (~opts: options, ~userInputs: JSON.t, ~events: array<CatalaRuntime.event>) => {
  let explanationSectionMap = events->Explanations.fromEvents
  Console.log2("explanationSectionMap", explanationSectionMap)
  Document.create({
    title: opts.title->Option.getUnsafe,
    creator: opts.creator->Option.getUnsafe,
    description: opts.description->Option.getUnsafe,
    sections: [
      // {
      //   children: [
      //     Paragraph.create'({
      //       text: opts.title->Option.getWithDefault("Explication individuelle du calcul"),
      //       heading: #Title,
      //       alignment: #center,
      //     }),
      //     Paragraph.create'({
      //       text: opts.description->Option.getUnsafe,
      //       heading: #Heading2,
      //       alignment: #center,
      //     }),
      //   ],
      // },
      getUserInputDocSection(
        ~userInputs,
        ~jsonSchema=opts.jsonSchema->Option.getWithDefault(JSON.Encode.null),
      ),
      explanationSectionMap->getResultDocSection,
      {
        children: events->Array.flatMap(eventToFileChild),
      },
    ],
    styles: {
      characterStyles: Styles.characterStyles,
    },
  })
  ->Packer.toBlob
  ->thenResolve(blob => {
    FileSaver.saveAs(blob, `${opts.filename}.docx`)
  })
  ->ignore
}
