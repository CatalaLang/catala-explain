/* This module provides functions to parse user input into a [Docx.fileChild] array. */

type rec t =
  | Section({title: string, items: t})
  | Fields(array<t>)
  | Field({name: string, value: t})
  | Array(array<t>)
  | LitEnum(string)
  | LitDate(string)
  | LitString(string)
  | LitNumber(float)
  | LitBool(bool)
  | LitNull

let fromJSON: (~json: JSON.t, ~schema: JSON.t) => t

module Docx: {
  let toFileChild: t => array<Docx.fileChild>
}
