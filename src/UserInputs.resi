/* This module provides functions to parse user input into a [Docx.fileChild] array. */

type rec t =
  | Section({title: string, items: t})
  | Fields(array<t>)
  | Field({
      name: string,
      value: t,
      // 'ui:tabLabel' in the uiSchema, if present, will be used as element
      // name for array items.
      tabLabel: option<string>,
    })
  | Array(array<t>)
  | LitEnum(string)
  | LitDate(string)
  | LitString(string)
  | LitNumber(float)
  | LitBool(bool)
  | LitNull

let fromJSON: (~json: JSON.t, ~schema: JSON.t, ~uiSchema: JSON.t) => t

module Docx: {
  let toFileChild: t => array<Docx.file_child>
}
