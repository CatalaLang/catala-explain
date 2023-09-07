/* This module provides functions to parse user input into a [Docx.fileChild] array. */

/** TODO: add documentation */
let parseVarDefs: (
  ~json: JSON.t,
  ~schema: JSON.t,
  ~uiSchema: JSON.t,
  ~keysToIgnore: array<string>,
) => array<CatalaRuntime.var_def>

let toTable: array<CatalaRuntime.var_def> => Docx.FileChild.t
