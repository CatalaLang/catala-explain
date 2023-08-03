open Docx

module DSFRColors = {
  type t = [
    | #blue_france_925
    | #blue_france_975
    | #red_marianne_925
    | #red_marianne_975
    | #green_emeraude_925
    | #green_emeraude_975
    | #grey_main_525
  ]

  let toHex = (color: t): string => {
    switch color {
    | #blue_france_925 => "#e3e3fd"
    | #blue_france_975 => "#f5f5fe"
    | #red_marianne_925 => "#fddede"
    | #red_marianne_975 => "#fef4f4"
    | #green_emeraude_925 => "#fde39c"
    | #green_emeraude_975 => "#fef6e3"
    | #grey_main_525 => "#666666"
    }
  }

  let getNextRowColor = (color: t) => {
    switch color {
    | #blue_france_925 => #blue_france_975
    | #blue_france_975 => #blue_france_925
    | #red_marianne_925 => #red_marianne_975
    | #red_marianne_975 => #red_marianne_925
    | #green_emeraude_925 => #green_emeraude_975
    | #green_emeraude_975 => #green_emeraude_925
    | c => c
    }
  }
}

let default: Document.defaultStyle = {
  let baseHeadingStyle: Document.style = {
    run: {
      font: "Marianne",
      bold: true,
      color: "161616",
    },
  }
  let baseHeadingStyleRun = baseHeadingStyle.run->Option.getExn
  {
    document: {
      run: {
        font: "Marianne",
        color: "3a3a3a",
        size: "8pt",
      },
    },
    heading1: {
      run: {
        ...baseHeadingStyleRun,
        size: "28pt",
      },
    },
    heading2: {
      run: {
        ...baseHeadingStyleRun,
        size: "24pt",
      },
    },
    heading3: {
      run: {
        ...baseHeadingStyleRun,
        size: "22pt",
      },
    },
    heading4: {
      run: {
        ...baseHeadingStyleRun,
        size: "18pt",
      },
    },
    heading5: {
      run: {
        ...baseHeadingStyleRun,
        size: "16pt",
      },
    },
    heading6: {
      run: {
        ...baseHeadingStyleRun,
        size: "14pt",
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

let characterStyles: array<Docx.Document.style> = [
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
      size: "10pt",
    },
  },
  {
    id: "ScopeName",
    name: "ScopeName",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: "Marianne",
      bold: true,
      color: "BB0066",
    },
  },
  {
    id: "VariableName",
    name: "VariableName",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: "Marianne",
      bold: true,
      size: "8pt",
      color: "161616",
    },
  },
  {
    id: "BooleanLiteral",
    name: "BooleanLiteral",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: "Marianne",
      bold: true,
      color: "BA2121",
    },
  },
  {
    id: "NumberLiteral",
    name: "NumberLiteral",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: "Marianne",
      bold: true,
      color: "008000",
    },
  },
  {
    id: "StringLiteral",
    name: "StringLiteral",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: "Marianne",
      bold: true,
      color: "BB0066",
    },
  },
  {
    id: "EnumLiteral",
    name: "EnumLiteral",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: "Marianne",
      bold: true,
      color: "BB0066",
    },
  },
  {
    id: "DateLiteral",
    name: "DateLiteral",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: "Marianne",
      bold: true,
      color: "0000FF",
    },
  },
  {
    id: "EmptyLiteral",
    name: "EmptyLiteral",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: "Marianne",
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
        color: "161616",
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
      size: "8pt",
    },
  },
]
