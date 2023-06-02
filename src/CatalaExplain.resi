type options = {
  title?: string,
  creator?: string,
  description?: string,
  /** The name of the file to generate. */
  filename: string,
  /** The schema of the user inputs, used for having explicit input form labels. */
  schema: JSON.t,
  uiSchema: JSON.t,
}

/**
* Generates a .DOCX file from a list of events.
*
* @param events The list of events to generate the .DOCX file from.
*/
let generate: (~opts: options, ~userInputs: JSON.t, ~events: array<CatalaRuntime.event>) => unit
