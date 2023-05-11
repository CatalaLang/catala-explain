open Docx
open Promise

let generate = (_events: array<CatalaRuntime.event>) => {
  let doc = Document.create({sections: []})
  doc
  ->Packer.toBlob
  ->thenResolve(blob => {
    Console.log(blob)
    FileSaver.saveAs(blob, "example.docx")
  })
  ->ignore
}
