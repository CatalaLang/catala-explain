open Docx
open CatalaRuntime
open Styles
open DSFRColors

// TODO: manage the language
let litLoggedValueToParagraphChild = (val: LoggedValue.t): paragraph_child => {
  switch val {
  | Bool(b) =>
    TextRun.create'({
      text: b ? "oui" : "non",
      style: "BooleanLiteral",
    })
  | Money(m) =>
    TextRun.create'({
      text: `${m->Float.toString}€`,
      style: "NumberLiteral",
    })
  | Decimal(f) =>
    TextRun.create'({
      text: f->Float.toString,
      style: "NumberLiteral",
    })
  | Integer(i) =>
    TextRun.create'({
      text: i->Int.toString,
      style: "NumberLiteral",
    })
  | Date(d) =>
    TextRun.create'({
      text: d
      ->Date.fromString
      ->Date.toLocaleDateStringWithLocaleAndOptions("fr-FR", {dateStyle: #long}),
      style: "DateLiteral",
    })
  | Duration(d) =>
    TextRun.create'({
      text: d->Utils.durationToString,
      style: "DateLiteral",
    })
  | Enum(_, (name, Unit)) =>
    TextRun.create'({
      text: name,
      style: "EnumLiteral",
    })
  | Array([]) =>
    TextRun.create'({
      text: "∅",
      style: "ArrayLiteral",
    })
  | _ => Js.Exn.raiseError(`Should be a literal logged value`)
  }
}

let getNormalTableCell = (
  ~columnSpan=1,
  ~borders: TableCell.table_cell_borders_options={},
  ~bgColor: DSFRColors.t,
  children,
): TableCell.t => {
  TableCell.create({
    shading: {fill: bgColor->toHex},
    borders,
    columnSpan,
    children,
  })
}

let getNormalTableCellParagraph = (~alignment: alignment_type=#left, children) => {
  Paragraph.create'({
    spacing: {before: 40, after: 40},
    alignment,
    children,
  })
}

let getNormalTextTableCell = (
  ~borders: TableCell.table_cell_borders_options={},
  ~bgColor: DSFRColors.t,
  ~columnSpan,
  text,
): TableCell.t => {
  [getNormalTableCellParagraph([TextRun.create(text)])]->getNormalTableCell(
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
    TableRow.create({
      children: [
        TableCell.create({
          children: [getNormalTableCellParagraph([Utils.getLawHeadingBreadcrumbsLink(pos)])],
          width: {size: 100.0, _type: #pct},
          shading: {
            fill: bgColor->toHex,
          },
          borders: {
            top: {
              style: #dotted,
              size: 0.25,
              color: #grey_main_525->toHex,
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
  ~borders: TableCell.table_cell_borders_options={},
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
    TableCell.create({
      children: [],
      width: {size: 4.0, _type: #pct},
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
  let borders: TableCell.table_cell_borders_options =
    lawHeadingOpt->Option.isSome ? {bottom: {style: #none}} : {}
  let varNameCell = getNormalTextTableCell(
    ~columnSpan=maxDepth - depth,
    ~borders,
    ~bgColor,
    name->Utils.lastExn,
  )
  let varDef = TableRow.create({
    children: depth
    ->getEmptyCellArray(~bgColor)
    ->Array.concat([varNameCell, value->litLogValueToCell(~bgColor, ~borders)]),
  })

  switch lawHeadingOpt {
  | Some(breadcrumpsRow) => [varDef, breadcrumpsRow]
  | None => [varDef]
  }
}

// TODO: could be factorized
let rec varDefToTableRow = (
  ~depth=0,
  ~maxDepth,
  ~bgColorRef: ref<DSFRColors.t>,
  {name, value, pos}: var_def,
): array<TableRow.t> => {
  switch value {
  | Enum(_, (_, val)) => {
      // Unwrap the enum
      let varDefWithoutInfos = Utils.getVarDefWithoutInfos(name, val)
      if val->Utils.isLitLoggedValue {
        litVarDefToTableRow(~maxDepth, ~depth, ~bgColor=bgColorRef.contents, varDefWithoutInfos)
      } else {
        varDefToTableRow(~maxDepth, ~depth, ~bgColorRef, varDefWithoutInfos)
      }
    }
  | Struct(structName, fields) => {
      let bgColor = bgColorRef.contents

      let varNameRow = TableRow.create({
        children: depth
        ->getEmptyCellArray(~bgColor)
        ->Array.concat([
          getNormalTableCell(
            ~columnSpan=maxDepth - depth + 1,
            ~borders={bottom: {style: #none}},
            ~bgColor,
            [
              getNormalTableCellParagraph([
                TextRun.create'({text: name->Utils.lastExn, bold: true}),
                TextRun.create'({
                  text: ` (de type ${structName->List.toArray->Array.joinWith(".")})`,
                  italics: true,
                  bold: true,
                }),
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
          let varDefWithoutInfos = Utils.getVarDefWithoutInfos(list{field}, value)

          bgColorRef := bgColorRef.contents->DSFRColors.getNextRowColor
          if value->Utils.isLitLoggedValue {
            litVarDefToTableRow(
              ~maxDepth,
              ~depth=depth + 1,
              ~bgColor=bgColorRef.contents,
              varDefWithoutInfos,
            )
          } else {
            varDefToTableRow(~maxDepth, ~depth=depth + 1, ~bgColorRef, varDefWithoutInfos)
          }
        })

      let emptyRow = TableRow.create({
        children: [
          TableCell.create({
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
        let varDefWithoutInfos = Utils.getVarDefWithoutInfos(
          name->List.map(n => n ++ " (élément " ++ elemNb.contents->Int.toString ++ ")"),
          value,
        )

        bgColorRef := bgColorRef.contents->DSFRColors.getNextRowColor
        if value->Utils.isLitLoggedValue {
          litVarDefToTableRow(~maxDepth, ~depth, ~bgColor=bgColorRef.contents, varDefWithoutInfos)
        } else {
          varDefToTableRow(~maxDepth, ~depth, ~bgColorRef, varDefWithoutInfos)
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