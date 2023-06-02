/** Basic integer identifiers. */
module Int: {
  module type Id = {
    type t = int
    let root: t
    let fresh: unit => t
  }

  /** Create a new identifier module. */
  module Make: () => Id
}
