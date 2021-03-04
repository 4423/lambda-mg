(* MakeSet functor example *)
module type EQ = sig
  type t
  val eq: t -> t -> bool
end
module type S = sig
  type elt
  type set
  val member: elt -> set -> bool
end

let makeSet (type a) = fun (m: (module EQ with type t = a) mcod) ->
  .<<(module struct
      type elt = $m.t
      type set = $m.t list

      let rec member elt set =
        match set with
          hd :: tl -> .~($m.eq) elt hd || member elt tl
        | _        -> false

     end: S with type elt = a and type set = a list)>>.
;;
let intset_cod = makeSet .<<(module struct
    type t = int
    let eq = fun x -> fun y -> x = y
end : EQ with type t = int)>>.
;;
let intset = (run_module intset_cod: S with type elt = int and type set = int list)
;;
