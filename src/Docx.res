module Document = {
  type t
  type input<'a> = {sections: array<'a>}

  @module("docx") @new external create: input<'a> => t = "Document"
}

type blob

module Packer = {
  type t

  @module("docx") external packer: t = "Packer"

  @send external toBlobBind: (t, Document.t) => promise<blob> = "toBlob"

  let toBlob = (doc: Document.t) => toBlobBind(packer, doc)
}

module FileSaver = {
  @module("file-saver") external saveAs: (blob, string) => unit = "saveAs"
}
