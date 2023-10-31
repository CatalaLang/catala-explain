open Docx
open CatalaRuntime
open Styles
open DSFRColors

// TODO: manage the language
let litLoggedValueToParagraphChild = (val: LoggedValue.t): ParagraphChild.t => {
  switch val {
  | Bool(b) =>
    TextRun.make'({
      text: b ? "oui" : "non",
      style: "BooleanLiteral",
    })
  | Money(m) =>
    TextRun.make'({
      text: `${m->Float.toString}€`,
      style: "NumberLiteral",
    })
  | Decimal(f) =>
    TextRun.make'({
      text: f->Float.toString,
      style: "NumberLiteral",
    })
  | Integer(i) =>
    TextRun.make'({
      text: i->Int.toString,
      style: "NumberLiteral",
    })
  | Date(d) =>
    TextRun.make'({
      text: d
      ->Date.fromString
      ->Date.toLocaleDateStringWithLocaleAndOptions("fr-FR", {dateStyle: #long}),
      style: "DateLiteral",
    })
  | Duration(d) =>
    TextRun.make'({
      text: d->Utils.durationToString,
      style: "DateLiteral",
    })
  | Enum(_, (name, Unit)) =>
    TextRun.make'({
      text: name,
      style: "EnumLiteral",
    })
  | Array([]) =>
    TextRun.make'({
      text: "aucun",
      style: "ArrayLiteral",
    })
  | _ => Js.Exn.raiseError(`Should be a literal logged value`)
  }
}

let getNormalTableCell = (
  ~columnSpan=1,
  ~borders: TableCell.tableCellBorders={},
  ~bgColor: DSFRColors.t,
  children,
): TableCell.t => {
  TableCell.make({
    shading: {fill: bgColor->toHex},
    borders,
    columnSpan,
    children,
    margins: {
      top: Spacing.xSmall *. 0.5,
      bottom: Spacing.xSmall *. 0.5,
    },
  })
}

let getNormalTableCellParagraph = (~alignment: AlignmentType.t=#left, children) => {
  Paragraph.make'({
    spacing: {before: 40.0, after: 40.0},
    alignment,
    children,
  })->Util.Types.ParagraphOrTable.fromParagraph
}

let getNormalTextTableCell = (
  ~borders: TableCell.tableCellBorders={},
  ~bgColor: DSFRColors.t,
  ~columnSpan,
  text,
): TableCell.t => {
  [getNormalTableCellParagraph([TextRun.make(text)])]->getNormalTableCell(
    ~bgColor,
    ~borders,
    ~columnSpan,
  )
}

let getLawHeadingInTableRow = (
  ~bgColor: DSFRColors.t,
  pos: option<CatalaRuntime.sourcePosition>,
): option<TableRow.t> => {
  switch pos {
  | Some(pos) =>
    TableRow.make({
      children: [
        TableCell.make({
          children: [getNormalTableCellParagraph([Utils.getLawHeadingBreadcrumbsLink(pos)])],
          width: {
            size: Utils.NumPctUni.fromFloat(100.0),
            type_: #pct,
          },
          shading: {
            fill: bgColor->toHex,
          },
          margins: {
            top: Spacing.xSmall *. 0.25,
            bottom: Spacing.xSmall *. 0.25,
          },
          borders: {
            top: {
              style: #dotted,
              size: 0.25,
              color: #grey->toHex,
            },
          },
        }),
      ],
    })->Some
  | None => None
  }
}

let litLogValueToCell = (
  ~bgColor: DSFRColors.t,
  ~borders: TableCell.tableCellBorders={},
  val: LoggedValue.t,
): TableCell.t => {
  getNormalTableCell(
    ~bgColor,
    ~borders,
    [getNormalTableCellParagraph(~alignment=#right, [val->litLoggedValueToParagraphChild])],
  )
}

let getEmptyCellArray = (~bgColor: DSFRColors.t, length: int): array<TableCell.t> => {
  Array.make(~length, 0)->Array.mapWithIndex((_, idx) => {
    TableCell.make({
      children: [],
      width: {size: Utils.NumPctUni.fromFloat(4.0), type_: #pct},
      shading: {fill: bgColor->toHex},
      borders: idx != 0
        ? {
            left: {
              style: #dotted,
              color: "#808080",
              size: 5.0,
            },
            right: {style: #none},
          }
        : {
            right: {style: #none},
          },
    })
  })
}

let litVarDefToTableRow = (
  ~depth=0,
  ~maxDepth,
  ~bgColor: DSFRColors.t,
  {name, value, pos}: var_def,
): array<TableRow.t> => {
  let lawHeadingOpt = getLawHeadingInTableRow(~bgColor, pos)
  let borders: TableCell.tableCellBorders =
    lawHeadingOpt->Option.isSome ? {bottom: {style: #none}} : {}
  let varNameCell = getNormalTextTableCell(
    ~columnSpan=maxDepth - depth,
    ~borders,
    ~bgColor,
    name->Utils.lastExn,
  )
  let varDef = TableRow.make({
    children: depth
    ->getEmptyCellArray(~bgColor)
    ->Array.concat([varNameCell, value->litLogValueToCell(~bgColor, ~borders)]),
  })

  switch lawHeadingOpt {
  | Some(breadcrumpsRow) => [varDef, breadcrumpsRow]
  | None => [varDef]
  }
}

let rec varDefToTableRow = (
  ~depth=0,
  ~maxDepth,
  ~bgColorRef: ref<DSFRColors.t>,
  {name, value, pos}: var_def,
): array<TableRow.t> => {
  switch value {
  | Enum(_, (_, value)) => {
      // Unwrap the enum
      let varDef = value->Utils.createVarDef(~name, ~pos)
      if value->Utils.isLitLoggedValue {
        litVarDefToTableRow(~maxDepth, ~depth, ~bgColor=bgColorRef.contents, varDef)
      } else {
        varDefToTableRow(~maxDepth, ~depth, ~bgColorRef, varDef)
      }
    }
  | Struct(structName, fields) => {
      let bgColor = bgColorRef.contents

      let varNameRow = TableRow.make({
        children: depth
        ->getEmptyCellArray(~bgColor)
        ->Array.concat([
          getNormalTableCell(
            ~columnSpan=maxDepth - depth + 1,
            ~borders={bottom: {style: #none}},
            ~bgColor,
            [
              getNormalTableCellParagraph([
                TextRun.make'({text: name->Utils.lastExn, bold: true}),
                if structName == list{} {
                  TextRun.make("")
                } else {
                  TextRun.make'({
                    text: ` (de type ${structName->List.toArray->Array.joinWith(".")})`,
                    italics: true,
                    bold: true,
                  })
                },
              ]),
            ],
          ),
        ]),
      })

      let valueRows =
        fields
        ->List.toArray
        ->Array.filter(((_, value)) => Utils.loggedValueIsEmbeddable(value))
        ->Array.sort(((_, v1), (_, v2)) => Utils.loggedValueCompare(v1, v2))
        ->Array.flatMap(((field, value)) => {
          let varDefWithoutPos = value->Utils.createVarDef(~name=list{field})

          bgColorRef := bgColorRef.contents->DSFRColors.getNextRowColor
          if value->Utils.isLitLoggedValue {
            litVarDefToTableRow(
              ~maxDepth,
              ~depth=depth + 1,
              ~bgColor=bgColorRef.contents,
              varDefWithoutPos,
            )
          } else {
            varDefToTableRow(~maxDepth, ~depth=depth + 1, ~bgColorRef, varDefWithoutPos)
          }
        })

      let emptyRow = TableRow.make({
        children: [
          TableCell.make({
            children: [],
            columnSpan: maxDepth,
          }),
        ],
      })

      switch getLawHeadingInTableRow(~bgColor, pos) {
      | Some(breadcrumpsRow) => [emptyRow, varNameRow, breadcrumpsRow]
      | None => [emptyRow, varNameRow]
      }->Array.concat(valueRows)
    }
  | Array(l) => {
      let elemNb = ref(0)
      l->Array.flatMap(value => {
        elemNb := elemNb.contents + 1
        let varDef =
          value->Utils.createVarDef(
            ~name=name->List.map(n => n ++ " (élément " ++ elemNb.contents->Int.toString ++ ")"),
            ~pos,
          )

        bgColorRef := bgColorRef.contents->DSFRColors.getNextRowColor
        if value->Utils.isLitLoggedValue {
          litVarDefToTableRow(~maxDepth, ~depth, ~bgColor=bgColorRef.contents, varDef)
        } else {
          varDefToTableRow(~maxDepth, ~depth, ~bgColorRef, varDef)
        }
      })
    }
  | _ => Js.Exn.raiseError(`Non-literal value is expected.`)
  }
}

let getTableRows = (
  ~bgColorRef: ref<DSFRColors.t>,
  ~maxDepth,
  defs: array<CatalaRuntime.var_def>,
): array<TableRow.t> => {
  defs
  ->Array.filter(({value}) => Utils.loggedValueIsEmbeddable(value))
  ->Array.sort((a, b) => Utils.loggedValueCompare(a.value, b.value))
  ->Array.flatMap(varDef => {
    bgColorRef := bgColorRef.contents->getNextRowColor
    if varDef.value->Utils.isLitLoggedValue {
      varDef->litVarDefToTableRow(~maxDepth, ~bgColor=bgColorRef.contents)
    } else {
      varDef->varDefToTableRow(~maxDepth, ~bgColorRef)
    }
  })
}

let getTable = (
  ~headingParagraph: Paragraph.t,
  ~maxDepth: int,
  ~bgColor: DSFRColors.t,
  ~contentRows: array<TableRow.t>,
): Table.t => {
  Table.make({
    columnWidths: Array.make(~length=maxDepth - 1, 4.0)->Array.concat([65.0, 25.0]),
    width: {size: Utils.NumPctUni.fromFloat(100.0), type_: #pct},
    alignment: #center,
    borders: {
      bottom: {style: #single},
    },
    rows: [
      TableRow.make({
        tableHeader: true,
        children: [
          TableCell.make({
            shading: {fill: bgColor->toHex},
            children: [headingParagraph->Util.Types.ParagraphOrTable.fromParagraph],
          }),
        ],
      }),
    ]->Array.concat(contentRows),
  })
}
