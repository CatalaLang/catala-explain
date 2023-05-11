// type options = {
//   fileName: string,
//   title: string,
// }

/**
* Generates a .DOCX file from a list of events.
*
* @param events The list of events to generate the .DOCX file from.
*/
let generate: (~userInputs: JSON.t, ~events: array<CatalaRuntime.event>) => unit
