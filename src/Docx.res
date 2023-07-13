type file_child

/** @see https://docx.js.org/api/types/ParagraphChild.html */
type paragraph_child

/** @see https://docx.js.org/api/enums/TextDirection.html */
type text_direction = [
  | /** Bottom to top, left to right */ #btLr
  | /** Left to right, top to bottom */ #lrTb
  | /** Top to bottom, right to left */ #tbR1
]

/**
 * @see https://docx.js.org/api/types/UniversalMeasure.html
 * 
 * NOTE(@EmileRolley): this is a weaker type, it should be a union of all
 * the possible values, but I don't know how to do that in ReScript.
 */
type universal_measure = string

/**
 * @see https://docx.js.org/api/types/Percentage.html
 * 
 * NOTE(@EmileRolley): this is a weaker type, it should be a union of all
 * the possible values, but I don't know how to do that in ReScript.
 */
type percentage = string

/**
 * @see https://docx.js.org/api/types/PositiveUniversalMeasure.html
 * 
 * NOTE(@EmileRolley): this is a weaker type, it should be a union of all
 * the possible values, but I don't know how to do that in ReScript.
 */
type positive_universal_measure = string

module NumberOrUniversalMeasure = {
  type t
  external number: float => t = "%identity"
  external universalMeasure: universal_measure => t = "%identity"
}

module NumberOrPositiveUniversalMeasure = {
  type t
  external number: float => t = "%identity"
  external universalMeasure: positive_universal_measure => t = "%identity"
}

module NumberOrPercentageOrUniversalMeasure = {
  type t
  external number: float => t = "%identity"
  external percentage: positive_universal_measure => t = "%identity"
  external universalMeasure: positive_universal_measure => t = "%identity"
}

/** @see https://docx.js.org/api/enums/BorderStyle.html */
type border_style = [
  | #dashed
  | #dashDotStroked
  | #dashSmallGap
  | #dotted
  | #dotDash
  | #dotDotDash
  | #double
  | #doubleWave
  | #inset
  | #nil
  | #none
  | #outset
  | #single
  | #thick
  | #thickThinLargeGap
  | #thickThinMediumGap
  | #thickThinSmallGap
  | #thinThickLargeGap
  | #thinThickMediumGap
  | #thinThickSmallGap
  | #thinThickThinLargeGap
  | #thinThickThinMediumGap
  | #thinThickThinSmallGap
  | #triple
  | #wave
]

type border_options = {
  color?: string,
  size?: float,
  space?: float,
  style: border_style,
}

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
  external create: string => paragraph_child = "TextRun"

  @module("docx") @new
  external create': options => paragraph_child = "TextRun"
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
    children: array<paragraph_child>,
  }

  @module("docx") @new
  external create: options => paragraph_child = "Bookmark"
}

/** @see https://docx.js.org/api/classes/InternalHyperlink.html */
module InternalHyperlink = {
  type options = {
    anchor: string,
    children: array<paragraph_child>,
  }

  @module("docx") @new
  external create: options => paragraph_child = "InternalHyperlink"
}

/** @see https://docx.js.org/api/classes/PageReference.html */
module PageReference = {
  type options = {
    hyperlink?: bool,
    useRelativePosition?: bool,
  }

  @module("docx") @new
  external create: string => paragraph_child = "PageReference"

  @module("docx") @new
  external create': (string, options) => paragraph_child = "PageReference"
}

/** @see https://docx.js.org/api/classes/ExternalHyperlink.html */
module ExternalHyperlink = {
  type options = {
    link: string,
    children: array<paragraph_child>,
  }

  @module("docx") @new
  external create: options => paragraph_child = "ExternalHyperlink"
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
  external create: (string, properties) => file_child = "TableOfContents"

  @module("docx") @new
  external create': properties => file_child = "TableOfContents"
}

/** @see https://docx.js.org/api/classes/Paragraph.html */
module Paragraph = {
  type bullet = {level: int}

  type options = {
    text?: string,
    heading?: HeadingLevel.t,
    // border?: border_options,
    children?: array<paragraph_child>,
    bullet?: bullet,
    style?: string,
    alignment?: AlignmentType.t,
    pageBreakBefore?: bool,
  }

  @module("docx") @new
  external create: string => file_child = "Paragraph"

  @module("docx") @new
  external create': options => file_child = "Paragraph"
}

/** @see https://docx.js.org/api/types/ITableWidthProperties.html */
type table_width = {
  size: NumberOrPercentageOrUniversalMeasure.t,
  @as("type") _type: [#auto | #nil | #pct | #dxa],
}

/** @see https://docx.js.org/api/classes/TableCell.html */
module TableCell = {
  type t

  type table_cell_borders_options = {
    top?: border_options,
    right?: border_options,
    bottom?: border_options,
    left?: border_options,
    end?: border_options,
    start?: border_options,
  }

  type options = {
    // NOTE(@EmileRolley): children should only be Paragraph or Table
    children: array<file_child>,
    borders?: table_cell_borders_options,
    columnSpan?: float,
    rowSpan?: float,
    textDirection?: text_direction,
    verticalAlign?: [#bottom | #center | #top],
    verticalMerge?: [#continue | #restart],
    width?: table_width,
  }

  @module("docx") @new
  external create: options => t = "TableCell"
}

/** @see https://docx.js.org/api/classes/TableRow.html */
module TableRow = {
  type t

  type height_options = {
    rule: [#auto | #atLeast | #exact],
    value: NumberOrPositiveUniversalMeasure.t,
  }

  type options = {
    children: array<TableCell.t>,
    canSplit?: bool,
    height?: height_options,
    tableHeader?: bool,
  }

  @module("docx") @new
  external create: options => t = "TableRow"
}

/** @see https://docx.js.org/api/classes/Table.html */
module Table = {
  type table_borders_options = {
    insideHorizontal?: border_options,
    insideVertical?: border_options,
    top?: border_options,
    left?: border_options,
    right?: border_options,
    bottom?: border_options,
  }

  type table_float_options = {
    absoluteHorizontalPosition?: NumberOrUniversalMeasure.t,
    absoluteVerticalPosition?: NumberOrUniversalMeasure.t,
    bottomFromText?: NumberOrPositiveUniversalMeasure.t,
    leftFromText?: NumberOrPositiveUniversalMeasure.t,
    rightFromText?: NumberOrPositiveUniversalMeasure.t,
    topFromText?: NumberOrPositiveUniversalMeasure.t,
  }

  type options = {
    alignment?: AlignmentType.t,
    borders?: table_borders_options,
    columnWidths?: array<float>,
    float?: table_float_options,
    layout?: [#autofit | #fixed],
    rows: array<TableRow.t>,
    style?: string,
    width?: table_width,
  }

  @module("docx") @new
  external create: options => file_child = "Table"
}

/** @see https://docx.js.org/api/interfaces/IHeaderOptions.html */
type header_options = {
  // NOTE(@EmileRolley): should only accept Paragraph or Table
  children: array<file_child>,
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
    children: array<file_child>,
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
