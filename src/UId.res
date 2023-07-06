module type Id = {
  type t = int

  let root: t
  let reset: unit => unit
  let fresh: unit => t
  let compare: (t, t) => int
}

module Int = {
  module Make = (): Id => {
    type t = int

    let root = -1

    let id = ref(0)

    let reset = () => id := 0

    let fresh = () => {
      id := id.contents + 1
      id.contents
    }
    let compare = (a, b) => a - b
  }
}
