(*
  ref: https://gist.github.com/dhil/55cf406865209ab945d8ba1484ea615c   
*)

(*
  definition of 'a fix, translate in Rust:

  enum fix<A> {
    Fix(Box<dyn Fn(fix<A>) -> A>)
  }   
*)
type 'a fix = Fix of ('a fix -> 'a)

(*
  fn wrap<A>(x: Box<dyn Fn(Fix<A>) -> A>) -> fix<A> {
    fix::Fix(x)
  }   
*)
let fold x = Fix x
let unfold (Fix x) = x

let y : 'a 'b. (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  = fun f ->
  let g x a = 
    f ((unfold x) x) a
  in 
    g (fold g)

let fact self n = if n = 0 then 1 else n * self (n - 1)

let _ =
  let result = y fact 10 in
  Printf.printf "%d\n%!" result;