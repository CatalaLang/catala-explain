module type Id = {
  type t = int
  let root: t
  let reset: unit => unit
  let fresh: unit => t
  let compare: (t, t) => float
}

/** Basic integer identifiers. */
module Int: {
  /** Create a new identifier module. */
  module Make: () => Id
}
