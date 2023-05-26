open Docx

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
        size: 16,
      },
    },
    heading1: {
      run: {
        ...baseHeadingStyleRun,
        size: 34,
      },
    },
    heading2: {
      run: {
        ...baseHeadingStyleRun,
        size: 29,
      },
    },
    heading3: {
      run: {
        ...baseHeadingStyleRun,
        size: 25,
      },
    },
    heading4: {
      run: {
        ...baseHeadingStyleRun,
        size: 22,
      },
    },
    heading5: {
      run: {
        ...baseHeadingStyleRun,
        size: 20,
      },
    },
    heading6: {
      run: {
        ...baseHeadingStyleRun,
        size: 18,
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
    id: "ScopeName",
    name: "ScopeName",
    basedOn: "Normal",
    next: "Normal",
    quickFormat: true,
    run: {
      font: "Fira Mono",
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
      font: "Fira Mono",
      bold: true,
      size: 16,
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
      font: "Fira Mono",
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
      font: "Fira Mono",
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
      font: "Fira Mono",
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
      font: "Fira Mono",
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
      font: "Fira Mono",
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
      font: "Fira Mono",
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
]
