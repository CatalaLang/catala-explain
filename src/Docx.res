type fileChild

/** @see https://docx.js.org/api/types/ParagraphChild.html */
type paragraphChild

/** @see https://docx.js.org/api/enums/PageNumber.html */
module PageNumber = {
  type t

  @module("docx") @scope("PageNumber")
  external current: t = "CURRENT"

  @module("docx") @scope("PageNumber")
  external totalPages: t = "TOTAL_PAGES"

  @module("docx") @scope("PageNumber")
  external totalInSection: t = "TOTAL_PAGES_IN_SECTION"
}

module TextRun = {
  /** @see https://docx.js.org/api/interfaces/IRunOptions.html#children */
  module Children = {
    type t
    external string: string => t = "%identity"
    external pageNumber: PageNumber.t => t = "%identity"
  }

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
    // NOTE(@EmileRolley): little hack to accept polymorphic array
    children?: array<Children.t>,
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

/** @see https://docx.js.org/api/classes/PageReference.html */
module PageReference = {
  type options = {
    hyperlink?: bool,
    useRelativePosition?: bool,
  }

  @module("docx") @new
  external create: string => paragraphChild = "PageReference"

  @module("docx") @new
  external create': (string, options) => paragraphChild = "PageReference"
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

/** @see https://docx.js.org/api/interfaces/IHeaderOptions.html */
type header_options = {
  // NOTE(@EmileRolley): should only accept Paragraph or Table
  children: array<fileChild>,
}

/** @see https://docx.js.org/api/classes/Header.html */
module Header = {
  type t

  @module("docx") @new
  external create: header_options => t = "Header"
}

/** @see https://docx.js.org/api/classes/Footer.html */
module Footer = {
  type t

  @module("docx") @new
  external create: header_options => t = "Footer"
}

module Document = {
  type t

  type section_properties = {}

  type footers = {
    default?: Footer.t,
    first?: Footer.t,
    even?: Footer.t,
  }

  type headers = {
    default?: Header.t,
    first?: Header.t,
    even?: Header.t,
  }

  type section = {
    properties?: section_properties,
    headers?: headers,
    footers?: footers,
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
