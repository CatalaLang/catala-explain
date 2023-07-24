open Docx
open CatalaRuntime

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

let getSectionTitle = (infos: information): string => {
  switch infos->List.reverse->List.head {
  | Some(name) => name
  | None => "_"
  }
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

let rec loggedValueKindToText = (value: LoggedValue.t): string => {
  switch value {
  | Enum(_, (name, v)) if v != Unit => name
  | Struct(infos, _) => infos->lastExn
  | Array(elems) =>
    if elems->Array.length == 0 {
      "ensemble vide"
    } else {
      "ensemble d'" ++ elems->Array.getUnsafe(0)->loggedValueKindToText
    }
  | val =>
    Js.Exn.raiseError(
      "Expected a struct or an enum with a value got" ++ LoggedValue.loggedValueToString(val, 0),
    )
  }
}

let getLawHeadingBreadcrumbsLink = (
  {filename, start_line, end_line, law_headings}: sourcePosition,
): paragraph_child => {
  ExternalHyperlink.create({
    children: [
      TextRun.create'({
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

let getVarDefWithoutInfos = (field: string, v: LoggedValue.t): var_def => {
  name: list{field},
  value: v,
  pos: None,
  fun_calls: None,
  io: {io_input: NoInput, io_output: false},
}
