module Int = {
  module type Id = {
    type t = int

    let root: t
    let fresh: unit => t
  }

  module Make = (): Id => {
    type t = int

    let root = -1

    let id = ref(0)

    let fresh = () => {
      id := id.contents + 1
      id.contents
    }
  }
}
