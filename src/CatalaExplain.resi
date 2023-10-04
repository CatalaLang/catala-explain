type options = {
  title?: string,
  creator?: string,
  description?: string,
  /**
   * Key from user inputs to ignore (e.g 'identifiant' in array items).
   * 
   * @note When parsing user inputs, value are replaced by their provided 'title'
   * fields in the json schema used for the form.
   */
  keysToIgnore?: array<string>,
  /* @param selectedOutput The output to show as the result of the computation. */
  selectedOutput?: CatalaRuntime.information,
}

/**
* Generates a .DOCX file from a list of events.
*
* @param events The list of events to show in the document.
* @param userInputs The user inputs use to compute the events.
* @param schema The schema of the user inputs, used for having explicit input form labels.
* @param opts The options used to generate the document.
*/
let generate: (
  ~events: array<CatalaRuntime.event>,
  ~userInputs: JSON.t,
  ~schema: JSON.t,
  ~opts: options,
) => Docx.Document.t
