type fileChild

/** @see https://docx.js.org/api/types/ParagraphChild.html */
type paragraphChild

module Document = {
  type t

  type section_properties = {}

  type section = {
    properties?: section_properties,
    children: array<fileChild>,
  }

  type options = {
    creator?: string,
    description?: string,
    title?: string,
    sections: array<section>,
  }

  @module("docx") @new
  external create: options => t = "Document"
}

module HeadingLevel = {
  type t

  @module("docx") @val
  external headingLevel: 'a = "HeadingLevel"

  let h1 = headingLevel["HEADING_1"]
  let h2 = headingLevel["HEADING_2"]
  let h3 = headingLevel["HEADING_3"]
  let h4 = headingLevel["HEADING_4"]
  let h5 = headingLevel["HEADING_5"]
  let title = headingLevel["TITLE"]
}

module TextRun = {
  /** @see https://docx.js.org/api/interfaces/IRunOptions.html */
  type options = {
    text?: string,
    allCaps?: bool,
    bold?: bool,
    break?: int,
    italics?: bool,
    font?: string,
    size?: int,
  }

  @module("docx") @new
  external create: string => paragraphChild = "TextRun"

  @module("docx") @new
  external create': options => paragraphChild = "TextRun"
}

module AlignmentType = {
  /** @see https://docx.js.org/api/enums/AlignmentType.html */
  type t

  @module("docx") @val
  external alignmentType: 'a = "AlignmentType"

  let both = alignmentType["BOTH"]
  let center = alignmentType["CENTER"]
  let distribute = alignmentType["DISTRIBUTE"]
  let end = alignmentType["END"]
  let highKashida = alignmentType["HIGH_KASHIDA"]
  let justified = alignmentType["JUSTIFIED"]
  let left = alignmentType["LEFT"]
  let lowKashida = alignmentType["LOW_KASHIDA"]
  let mediumKashida = alignmentType["MEDIUM_KASHIDA"]
  let numTab = alignmentType["NUM_TAB"]
  let right = alignmentType["RIGHT"]
  let start = alignmentType["START"]
  let thaiDistribute = alignmentType["THAI_DISTRIBUTE"]
}

module Paragraph = {
  type bullet = {level: int}

  type options = {
    text?: string,
    heading?: HeadingLevel.t,
    // border?: border_options,
    children?: array<paragraphChild>,
    bullet?: bullet,
    style?: string,
    alignment?: AlignmentType.t,
  }

  @module("docx") @new
  external create: string => fileChild = "Paragraph"

  @module("docx") @new
  external create': options => fileChild = "Paragraph"
}

type blob

module Packer = {
  @module("docx") @scope("Packer")
  external toBlob: Document.t => promise<blob> = "toBlob"
}

module FileSaver = {
  @module("file-saver")
  external saveAs: (blob, string) => unit = "saveAs"
}
