
<div align="center">
  <h3 align="center">
	<big>Catala Explain <i>from trace</i></big>
  </h3>
  <p align="center">
   <a href="https://github.com/CatalaLang/catala-explain/issues">Report Bug</a>
   •
   <a href="https://github.com/CatalaLang/catala-dsfr">Example</a>
  </p>

<!-- ![CI][ci-link] ![NPM][npm-link] -->
A module for explicability from the trace of a catala program.

<img width="600" src="https://github.com/CatalaLang/catala-explain/assets/44124798/c9478ce4-e204-42a2-b4fd-d600a378a9be"/>
<!-- ![Screenshot from 2023-10-04 11-24-52](https://github.com/CatalaLang/catala-explain/assets/44124798/c9478ce4-e204-42a2-b4fd-d600a378a9be) -->


</div>

## Installation

To add this module to your ReScript project, install the following dependencies:

```sh
yarn add -D @catala-lang/catala-explain rescript-docx
```

Then add them to the `bs-dependencies` of your `bsconfig.json`:

```diff
 {
   ...
   "bs-dependencies": [
+    "@catala-lang/catala-explain"
+    "rescript-docx"
   ]
}
```

> ℹ️ You need to install
> [`rescript-docx`](https://github.com/CatalaLang/rescript-docx) to manage the
> generated .docx file.

## Usage

This module is meant to be used as a library. It provides a function
`CatalaExplain.generate` that takes the user's input and the trace of a catala
program and generates a .docx file containing the explanation of the result of
the program.

```rescript
let doc = CatalaExplain.generate(
    // A JSON object containing the user's input used to generate the document
    ~userInputs=formData,
    // The trace of the catala program
    ~events=CatalaFrenchLaw.retrieveEventsSerialized()->CatalaRuntime.deserializedEvents,
    // The options for the document
    ~opts={
        title: "Titre de la décision",
        description: "Description du document",
        creator: "Créateur du document",
        // The schema of the user inputs form, used for having explicit input form labels.
        schema: WebAssets.schema,
        // Key from user inputs to ignore (e.g 'identifiant' in array items), instead
        // of using the 'title' field of the schema.
        keysToIgnore: WebAssets.keysToIgnore,
        // The output to show as the result of the computation
        selectedOutput: WebAssets.selectedOutput,
    },
)

// Example of how to save the generated document
// 
// Where `FileSaver` is a minimal wrapper around the `file-saver` package
doc
->Docx.Packer.toBlob
->Promise.thenResolve(blob => {
    FileSaver.saveAs(blob, `explication-decision.docx`)
})
->ignore
```

> ℹ️ You can find a complete example of usage in the
> [`catala-dsfr`](https://github.com/CatalaLang/catala-dsfr) repository.

## Sponsors

This library has been developed during a research project funded by the
[_mission logiciels libres et communs numériques_](https://www.code.gouv.fr/)
of the [_direction interministérielle du
numérique_](https://www.numerique.gouv.fr/) in collaboration with the
[Catala](https://catala-lang.org/) project.
