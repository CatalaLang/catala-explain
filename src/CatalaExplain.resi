type options = {
  title?: string,
  creator?: string,
  description?: string,
  /** The schema of the user inputs, used for having explicit input form labels. */
  schema: JSON.t,
  /**
   * Key from user inputs to ignore (e.g 'identifiant' in array items).
   *
   * @note When parsing user inputs, value are replaced by their provided
   * 'title' fields in the json schema used for the form.
   */
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
let generate: (
  ~userInputs: JSON.t,
  ~events: array<CatalaRuntime.event>,
  ~opts: options,
) => Docx.Document.t
