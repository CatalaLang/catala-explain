/* Core logic generating docx from Catala events */

module SectionId: UId.Id

/**
  A section is a step of the computation, with a title, a list of inputs and
  outputs, and a list of explanations.

  Each sub-scope call or function call is considered a section. The top-level
  scope is also a section (with the [id == SectionId.root]).

  An explanation is either a definition of a variable, or a reference to another
  section.
*/
type rec section = {
  id: SectionId.t,
  parent: SectionId.t,
  title: string,
  inputs: array<CatalaRuntime.var_def>,
  outputs: array<CatalaRuntime.var_def>,
  explanations: array<explanation>,
}
and explanation = Def(CatalaRuntime.var_def) | Ref(SectionId.t)

type sectionMap = RescriptCore.Map.t<SectionId.t, section>

let fromEvents: array<CatalaRuntime.event> => sectionMap

module Docx: {
  let outputToFileChilds: sectionMap => array<Docx.file_child>
  let explanationsToFileChilds: sectionMap => array<Docx.file_child>
}
