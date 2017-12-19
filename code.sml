(* シグネチャ *)
signature STACK =
sig
  type 'a stack
  val create : unit -> 'a stack
  val push : 'a * 'a stack -> 'a stack
  val pop : 'a stack -> 'a * 'a stack
end

(* モジュール、ストラクチャ *)
structure Stack : STACK =
struct
  type 'a stack = 'a list
  fun create () = nil
  fun push (x, s) = x :: s
  fun pop nil = raise Empty
    | pop (h::t) = (h, t)
end

(* REPLでの使用例
# val s = Stack.push (1, Stack.create ())
val s = [1] : int list
# val s = Stack.push (2, [0])
val s = [2, 0] : int list
*)

(* ファンクタ *)
signature MONOSTACK =
sig
  type t
  type stack
  val create : unit -> stack
  val push : t * stack -> stack
  val pop : stack -> t * stack
end
signature ELEM = sig type elem end
functor MakeMonoStack (E : ELEM) : MONOSTACK =
struct
  type t = E.elem
  type stack = t list
  fun create () = nil
  fun push (x, s) = x :: s
  fun pop nil = raise Empty
    | pop (h::t) = (h, t)
end
structure Elem = struct type elem = int end
structure MyStack = MakeMonoStack (Elem)
val s = MyStack.create ()
val s = MyStack.push (0, s)
val (x, s) = MyStack.pop s

(* 透明なシグネチャ制約 *)
structure MonoStack : MONOSTACK =
struct
  type t = int
  type stack = t list
  fun create () = nil
  fun push (x, s) = x :: s
  fun pop nil = raise Empty
    | pop (h::t) = (h, t)
end
val s = MonoStack.push (2, [0]);

(* 不透明なシグネチャ制約 *)
structure MonoStack :> MONOSTACK =
struct
  type t = int
  type stack = t list
  fun create () = nil
  fun push (x, s) = x :: s
  fun pop nil = raise Empty
    | pop (h::t) = (h, t)
end
val s = MonoStack.create ()
(* type error
val s = MonoStack.push (2, [0])
 *)

val s = MonoStack.create ()
(* type error
val s = MonoStack.push (1, s)
 *)

(* 一部の型だけ透明制約にする *)
structure MonoStack :> MONOSTACK where type t = int =
struct
  type t = int
  type stack = t list
  fun create () = nil
  fun push (x, s) = x :: s
  fun pop nil = raise Empty
    | pop (h::t) = (h, t)
end
val s = MonoStack.create ()
val s = MonoStack.push (1, s)

(* ファンクタでのシグネチャ制約の例 *)
functor MakeMonoStack (E : ELEM) :> MONOSTACK =
struct
  type t = E.elem
  type stack = t list
  fun create () = nil
  fun push (x, s) = x :: s
  fun pop nil = raise Empty
    | pop (h::t) = (h, t)
end
structure Elem = struct type elem = int end
structure MyStack = MakeMonoStack (Elem)
val s = MyStack.create ()
(* type error
val s = MyStack.push (0, s)
val (x, s) = MyStack.pop s
*)

functor MakeMonoStack (E : ELEM) :> MONOSTACK where type t = E.elem =
struct
  type t = E.elem
  type stack = t list
  fun create () = nil
  fun push (x, s) = x :: s
  fun pop nil = raise Empty
    | pop (h::t) = (h, t)
end
structure Elem = struct type elem = int end
structure MyStack = MakeMonoStack (Elem)
val s = MyStack.create ()
val s = MyStack.push (0, s)
val (x, s) = MyStack.pop s

(* シグネチャの展開 *)
signature MONOSTACK' =
sig
  include MONOSTACK
  val isEmpty : stack -> bool
end
(* モジュールの展開 *)
structure MonoStack' =
struct
  open MonoStack
  fun isEmpty stack =
    let val _ = pop stack
    in false end
    handle Empty => true
end
val b = MonoStack'.isEmpty (MonoStack'.create ())

(* 型の共有 *)
signature AB =
sig
  structure A : sig type ta end
  structure B : sig type tb end
  sharing type A.ta = B.tb
end
