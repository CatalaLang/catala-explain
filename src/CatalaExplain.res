open Docx
open Promise

let rec eventToFileChild = (event: CatalaRuntime.event) => {
  open CatalaRuntime

  switch event {
  | SubScopeCall({sname, sbody}) =>
    [
      Paragraph.create({
        text: "SubScopeCall: " ++ sname->List.toArray->Array.joinWith(" "),
      }),
    ]->Array.concat(sbody->List.toArray->Array.flatMap(eventToFileChild))
  | _ => []
  }
}

let generate = (events: array<CatalaRuntime.event>) => {
  let doc = Document.create({
    sections: [
      {
        children: events->Array.flatMap(eventToFileChild),
      },
    ],
  })
  doc
  ->Packer.toBlob
  ->thenResolve(blob => {
    Console.log(blob)
    FileSaver.saveAs(blob, "example.docx")
  })
  ->ignore
}
