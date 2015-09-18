
type ('a, 'b) t = [ `Ok of 'a | `Error of 'b ]

let bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t = fun m f -> match m with
  | `Ok x -> f x
  | `Error _ as y -> y

let return x = `Ok x

let (>>=) = bind
