type fileChild

module Document = {
  type t

  type section<'a> = {
    properties?: option<'a>,
    children: array<fileChild>,
  }

  type input<'a> = {sections: array<section<'a>>}

  @module("docx") @new external create: input<'a> => t = "Document"
}

module Paragraph = {
  type input = {
    text?: string,
    children?: array<fileChild>,
  }

  @module("docx") @new external create: input => fileChild = "Paragraph"
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
