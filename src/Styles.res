open Docx
open Util

module DSFRColors = {
  type t = [
    | #blue
    | #blue_light
    | #red
    | #red_light
    | #yellow
    | #yellow_light
    | #grey
  ]

  let toHex = (color: t): string => {
    switch color {
    | #blue => "#e9edfe"
    | #blue_light => "#f5f5fe"
    | #red => "#fee9e7"
    | #red_light => "#fef4f4"
    | #yellow => "#feecc2"
    | #yellow_light => "#fef6e3"
    | #grey => "#666666"
    }
  }

  let getNextRowColor = (color: t) => {
    switch color {
    | #blue => #blue_light
    | #blue_light => #blue
    | #red => #red_light
    | #red_light => #red
    | #yellow => #yellow_light
    | #yellow_light => #yellow
    | c => c
    }
  }
}

module Spacing = {
  let base = 72.0 *. 20.0
  let large = 1.0 *. base
  let medium = 0.5 *. base
  let small = 0.25 *. base
  let xSmall = 0.1 *. base
}

let solidBorder: Docx.BorderOptions.t = {
  color: "auto",
  space: 1.0,
  size: 6.0,
  style: #single,
}

let marianneFont = Font.fromString("Marianne")
let posUniversalMeasure = Types.NumberOrPositiveUniversalMeasure.fromPositiveUniversalMeasure

let default: StylesOptions.defaultStylesOptions = {
  let baseHeadingStyle: StylesOptions.baseCharacterStyleOptions = {
    run: {
      font: marianneFont,
      bold: true,
      color: "161616",
    },
  }
  let baseHeadingStyleRun = baseHeadingStyle.run->Option.getExn
  {
    document: {
      run: {
        font: marianneFont,
        color: "3a3a3a",
        size: posUniversalMeasure("9pt"),
      },
    },
    heading1: {
      run: {
        ...baseHeadingStyleRun,
        size: posUniversalMeasure("24pt"),
      },
    },
    heading2: {
      run: {
        ...baseHeadingStyleRun,
        size: posUniversalMeasure("22pt"),
      },
    },
    heading3: {
      run: {
        ...baseHeadingStyleRun,
        size: posUniversalMeasure("18pt"),
      },
    },
    heading4: {
      run: {
        ...baseHeadingStyleRun,
        size: posUniversalMeasure("16pt"),
      },
    },
    heading5: {
      run: {
        ...baseHeadingStyleRun,
        size: posUniversalMeasure("14pt"),
      },
    },
    heading6: {
      run: {
        ...baseHeadingStyleRun,
        size: posUniversalMeasure("11pt"),
        bold: false,
      },
    },
    hyperlink: {
      run: {
        color: "161616",
        underline: {
          color: "161616",
          type_: #single,
        },
      },
    },
  }
}

let characterStyles: array<StylesOptions.characterStyleOptions> = [
  {
    id: "BoldText",
    name: "BoldText",
    basedOn: "Normal",
    next: "Normal",
    run: {
      bold: true,
    },
  },
  {
    id: "TableHeadingText",
    name: "TableHeadingText",
    basedOn: "BoldText",
    next: "Normal",
    quickFormat: true,
    run: {
      size: posUniversalMeasure("10pt"),
    },
  },
  {
    id: "ScopeName",
    name: "ScopeName",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: marianneFont,
      bold: true,
      color: "#BB0066",
    },
  },
  {
    id: "VariableName",
    name: "VariableName",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: marianneFont,
      bold: true,
      color: "#BB0066",
    },
  },
  {
    id: "BooleanLiteral",
    name: "BooleanLiteral",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: marianneFont,
      bold: true,
      color: "#e4794a",
    },
  },
  {
    id: "NumberLiteral",
    name: "NumberLiteral",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: marianneFont,
      bold: true,
      color: "#008000",
    },
  },
  {
    id: "StringLiteral",
    name: "StringLiteral",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: marianneFont,
      bold: true,
      color: "#BB0066",
    },
  },
  {
    id: "EnumLiteral",
    name: "EnumLiteral",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: marianneFont,
      bold: true,
      color: "#BB0066",
    },
  },
  {
    id: "DateLiteral",
    name: "DateLiteral",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: marianneFont,
      bold: true,
      color: "#0000FF",
    },
  },
  {
    id: "EmptyLiteral",
    name: "EmptyLiteral",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: marianneFont,
      italics: true,
    },
  },
  {
    id: "Hyperlink",
    name: "Hyperlink",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      underline: {
        color: "#161616",
        type_: #single,
      },
    },
  },
  {
    id: "NormalTableCellText",
    name: "NormalTableCellText",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      size: posUniversalMeasure("8pt"),
    },
  },
  {
    id: "ComputeStepToc",
    name: "ComputeStepToc",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      size: posUniversalMeasure("12pt"),
      bold: false,
    },
  },
]
