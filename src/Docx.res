type fileChild

/** @see https://docx.js.org/api/types/ParagraphChild.html */
type paragraphChild

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
    style?: string,
    color?: string,
  }

  @module("docx") @new
  external create: string => paragraphChild = "TextRun"

  @module("docx") @new
  external create': options => paragraphChild = "TextRun"
}

module AlignmentType = {
  /** 
   * @see https://docx.js.org/api/enums/AlignmentType.html 
   *
   * #both == JUSTIFIED
   */
  type t = [
    | #start
    | #center
    | #end
    | #both
    | #distribute
    | #left
    | #right
    | #both
    | #mediumKashida
    | #highKashida
    | #lowKashida
    | #thaiDistribute
    | #numTab
  ]
}

module HeadingLevel = {
  type t = [
    | #Heading1
    | #Heading2
    | #Heading3
    | #Heading4
    | #Heading5
    | #Heading6
    | #Title
  ]
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

module Document = {
  type t

  type section_properties = {}

  type section = {
    properties?: section_properties,
    children: array<fileChild>,
  }

  type style = {
    id: string,
    name: string,
    basedOn?: string,
    next?: string,
    quickFormat?: bool,
    run?: TextRun.options,
    paragraph?: Paragraph.options,
  }
  type styles = {paragraphStyles?: array<style>, characterStyles?: array<style>}

  type options = {
    creator?: string,
    description?: string,
    title?: string,
    styles?: styles,
    sections: array<section>,
  }

  @module("docx") @new
  external create: options => t = "Document"
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
