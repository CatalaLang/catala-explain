type fileChild

/** @see https://docx.js.org/api/types/ParagraphChild.html */
type paragraphChild

module TextRun = {
  type underline = {
    color?: string,
    @as("type")
    type_: [
      | #dash
      | #dashDotDotHeavy
      | #dashLong
      | #dashLongHeavy
      | #dotDash
      | #dotDotDash
      | #dotted
      | #dottedHeavy
      | #double
      | #single
      | #thick
      | #wave
      | #wavyDouble
      | #wavyHeavy
      | #words
    ],
  }
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
    underline?: underline,
  }

  @module("docx") @new
  external create: string => paragraphChild = "TextRun"

  @module("docx") @new
  external create': options => paragraphChild = "TextRun"
}

/**
@see https://docx.js.org/api/enums/AlignmentType.html 

#both == JUSTIFIED 
*/
module AlignmentType = {
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

/** @see https://docx.js.org/api/enums/HeadingLevel.html */
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

/** @see https://docx.js.org/api/classes/Bookmark.html */
module Bookmark = {
  type options = {
    id: string,
    children: array<paragraphChild>,
  }

  @module("docx") @new
  external create: options => paragraphChild = "Bookmark"
}

/** @see https://docx.js.org/api/classes/InternalHyperlink.html */
module InternalHyperlink = {
  type options = {
    anchor: string,
    children: array<paragraphChild>,
  }

  @module("docx") @new
  external create: options => paragraphChild = "InternalHyperlink"
}

/** @see https://docx.js.org/api/classes/ExternalHyperlink.html */
module ExternalHyperlink = {
  type options = {
    link: string,
    children: array<paragraphChild>,
  }

  @module("docx") @new
  external create: options => paragraphChild = "ExternalHyperlink"
}

/**
@see https://docx.js.org/api/classes/TableOfContents.html

NOTE(@EmileRolley): Doesn't seem to work (only tested with LibreOffice).

*/
module TableOfContents = {
  /** @see https://docx.js.org/api/interfaces/ITableOfContentsOptions.html */
  type properties = {
    hyperlink?: bool,
    headingStyleRange?: string,
  }

  @module("docx") @new
  external create: (string, properties) => fileChild = "TableOfContents"

  @module("docx") @new
  external create': properties => fileChild = "TableOfContents"
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
    id?: string,
    name?: string,
    basedOn?: string,
    next?: string,
    quickFormat?: bool,
    run?: TextRun.options,
    paragraph?: Paragraph.options,
  }

  type defaultStyle = {
    document?: style,
    heading1?: style,
    heading2?: style,
    heading3?: style,
    heading4?: style,
    heading5?: style,
    heading6?: style,
    hyperlink?: style,
    paragraphList?: style,
  }

  type styles = {
    default?: defaultStyle,
    paragraphStyles?: array<style>,
    characterStyles?: array<style>,
  }

  type features = {updateFields?: bool}

  type options = {
    creator?: string,
    description?: string,
    title?: string,
    styles?: styles,
    sections: array<section>,
    features?: features,
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
