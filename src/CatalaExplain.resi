type options = {
  title?: string,
  creator?: string,
  description?: string,
  /** The name of the file to generate. */
  filename: string,
  /** The schema of the user inputs, used for having explicit input form labels. */
  schema: JSON.t,
  uiSchema: JSON.t,
  /** Key from user inputs to ignore (e.g 'identifiant' in array items) */
  keysToIgnore: array<string>,
  /** The output to show as the result of the computation. */
  selectedOutput: CatalaRuntime.information,
}

/**
* Generates a .DOCX file from a list of events.
*
* @param userInputs The user inputs use to compute the events.
* @param events The list of events to show in the document.
* @param opts The options for the generation.
*/
let generate: (~userInputs: JSON.t, ~events: array<CatalaRuntime.event>, ~opts: options) => unit
