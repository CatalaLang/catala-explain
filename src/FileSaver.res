type blob

@module("file-saver")
external saveAs: (blob, string) => unit = "saveAs"
