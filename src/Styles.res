open Docx

module DsfrColors = {
  type t = [
    | #blue_france_925
    | #blue_france_975
  ]

  let toHex = (color: t): string => {
    switch color {
    | #blue_france_925 => "f5f5fe"
    | #blue_france_975 => "ffffff"
    }
  }

  let getNextRowColor = (color: t) => {
    switch color {
    | #blue_france_925 => #blue_france_975
    | #blue_france_975 => #blue_france_925
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
        size: "34pt",
      },
    },
    heading2: {
      run: {
        ...baseHeadingStyleRun,
        size: "15pt",
      },
    },
    heading3: {
      run: {
        ...baseHeadingStyleRun,
        size: "12pt",
      },
    },
    heading4: {
      run: {
        ...baseHeadingStyleRun,
        size: "22pt",
      },
    },
    heading5: {
      run: {
        ...baseHeadingStyleRun,
        size: "20pt",
      },
    },
    heading6: {
      run: {
        ...baseHeadingStyleRun,
        size: "18pt",
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
