open Docx
open CatalaRuntime

module NumPctUni = Util.Types.NumberOrPercentageOrUniversalMeasure

let lastExn = (l: list<'a>): 'a => l->List.reverse->List.headExn

let isDate = (str: string): bool => {
  str->Date.fromString->Date.toString != "Invalid Date"
}

let getNextHeadingLevel = (lvl: HeadingLevel.t): HeadingLevel.t => {
  switch lvl {
  | #Title => #Heading1
  | #Heading1 => #Heading2
  | #Heading2 => #Heading3
  | #Heading3 => #Heading4
  | #Heading4 => #Heading5
  | #Heading5 => #Heading6
  | _ => #Heading6
  }
}

let getSubScopeId = (~sep=".", name: information): string => {
  name->List.toArray->Array.joinWith(sep)
}

let getSectionTitle = (~size=None, scopeName: information) => {
  let nbSegments = scopeName->List.length
  scopeName
  ->List.toArray
  ->Array.mapWithIndex((segment, i) => {
    let isLast = i == nbSegments - 1
    switch size {
    | Some(n) if isLast =>
      TextRun.make'({
        text: segment,
        size: `${n->Int.toString}pt`,
        bold: true,
      })
    | Some(n) =>
      TextRun.make'({
        text: `${segment} > `,
        size: `${(n - 2)->Int.toString}pt`,
      })
    | None =>
      TextRun.make'({
        text: segment ++ (isLast ? "" : " > "),
        bold: isLast,
      })
    }
  })
}

@raises(Error.t)
let getJsErr = (opt: option<'a>, errMsg: string): 'a => {
  switch opt {
  | Some(x) => x
  | None => Js.Exn.raiseError(errMsg)
  }
}

let loggedValueIsEmbeddable = (value: LoggedValue.t): bool => {
  value != Unembeddable
}

let loggedValueOrder = (value: LoggedValue.t): int => {
  switch value {
  | Array(_) => 3
  | Struct(_) => 2
  | Enum(_, (_, v)) if v != Unit => 1
  | _ => 0
  }
}

let loggedValueCompare = (a: LoggedValue.t, b: LoggedValue.t): int =>
  loggedValueOrder(a) - loggedValueOrder(b)

let orderAndFilterEmpty = (values: array<LoggedValue.t>): array<LoggedValue.t> => {
  values
  ->Array.filter(val => !loggedValueIsEmbeddable(val))
  ->Array.sort((a, b) => loggedValueOrder(a) - loggedValueOrder(b))
}

let getLawHeadingBreadcrumbsLink = (
  {filename, start_line, end_line, law_headings}: sourcePosition,
): ParagraphChild.t => {
  ExternalHyperlink.make({
    children: [
      TextRun.make'({
        text: law_headings->Array.joinWith(" > "),
        size: "6pt",
        underline: {
          type_: #single,
        },
      }),
    ],
    link: `https://github.com/CatalaLang/catala/blob/master/${filename}#L${start_line->Int.toString}-L${end_line->Int.toString}`,
  })
}

/**
* The expected format of the @param d is "[<number> years, <number> months, <number> days]"
*/
let durationToString = (d: string) => {
  let days = d => d ++ (d == "1" ? " jour" : " jours")
  let years = d => d ++ (d == "1" ? " an" : " ans")
  let months = d => d ++ " mois"
  switch d->String.match(%re("/\[(\d+) years, (\d+) months, (\d+) days\]/")) {
  | Some([_, "0", "0", "0"]) => "0 jour"
  | Some([_, "0", "0", d]) => days(d)
  | Some([_, "0", m, "0"]) => months(m)
  | Some([_, "0", m, d]) => months(m) ++ " et " ++ days(d)
  | Some([_, y, "0", "0"]) => years(y)
  | Some([_, y, "0", d]) => years(y) ++ " et " ++ days(d)
  | Some([_, y, m, "0"]) => years(y) ++ " et " ++ months(m)
  | Some([_, y, m, d]) => years(y) ++ ", " ++ months(m) ++ " et " ++ days(d)
  | Some(_) | None => Js.Exn.raiseError("Got an invalid duration: " ++ d)
  }
}

let isArrayLoggedValue = (v: LoggedValue.t): bool => {
  switch v {
  | Array(_) => true
  | _ => false
  }
}

let getVarDefWithoutInfos = (name: list<string>, value: LoggedValue.t): var_def => {
  name,
  value,
  pos: None,
  fun_calls: None,
  io: {io_input: NoInput, io_output: false},
}

let isLitLoggedValue = (val: LoggedValue.t): bool => {
  switch val {
  | Enum(_, (_, val)) if val != Unit => false
  | Struct(_, l) if l->List.length != 0 => false
  | Array(l) if l->Array.length != 0 => false
  | Unembeddable => // TODO: handle unembeddable, which are functions and other stuff
    false
  | _ => true
  }
}

let getMaxDepth = (inputs: array<var_def>): int => {
  let rec loggedValueGetMaxDepth = (~depth=1, value: LoggedValue.t): int => {
    switch value {
    | Struct(_, l) =>
      l
      ->List.toArray
      ->Array.map(((_, value)) => value->loggedValueGetMaxDepth(~depth=depth + 1))
      ->Array.reduce(1, (a, b) => Math.Int.max(a, b))
    | Enum(_, (_, l)) if l->isLitLoggedValue => depth
    | Enum(_, (_, l)) =>
      // NOTE(@EmileRolley): we need to unwrap the enum first, because we
      // don't want to count the enum itself
      l->loggedValueGetMaxDepth(~depth)
    | Array(elems) =>
      elems
      ->Array.map(value => value->loggedValueGetMaxDepth(~depth))
      ->Array.reduce(1, (a, b) => Math.Int.max(a, b))
    | _ => depth
    }
  }

  inputs
  ->Array.map(({value}) => {
    if !(value->isLitLoggedValue) {
      value->loggedValueGetMaxDepth
    } else {
      1
    }
  })
  ->Array.reduce(1, (a, b) => Math.Int.max(a, b))
}
