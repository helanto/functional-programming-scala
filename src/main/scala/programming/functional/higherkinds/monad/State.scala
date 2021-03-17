package programming.functional.higherkinds.monad

/** == The State Monad ==
  * Pure functions cannot change global state, nor have access to global variables. They can only
  * perform some computations on input values and return some results. However, some problems are
  * inherently stateful, relying on some state that changes over time. The ''state monad'' comes
  * to the rescue.
  *
  * We represent '''stateful computations''' as a function from current state to a value and some new
  * state. This stateful computation, can be thought of as value with a context as well. The actual
  * value is the result, while the context is that we have to give some initial state to actually get
  * that result back.
  *
  * As an example let's consider a stack; we can push and pop elements from the top of the stack.
  * {{{
  *  import programming.functional.higherkinds.monad.State
  *  import programming.functional.higherkinds.Monad._
  *
  *  type Stack = List[Int]
  *  implicit val stackStateMonad = stateMonad[Stack]
  *
  *  // Pops an element from the top of the stack.
  *  def pop(): State[Stack, Int] = State(stack => (stack.tail, stack.head))
  *  // Pushes an element to the top of the stack.
  *  def push(el: Int): State[Stack, Unit] = State(stack => (el :: stack, ()))
  *
  *  val stack = for {
  *    zero <- stackStateMonad.point[Int](0)
  *    one  <- stackStateMonad.point[Int](1)
  *    two  <- stackStateMonad.point[Int](2)
  *    _    <- push(zero) // 5 -> 4 -> 3 -> 0
  *    _    <- push(one)  // 5 -> 4 -> 3 -> 0 -> 1
  *    fst  <- pop()      // 5 -> 4 -> 3 -> 0
  *    _    <- push(two)  // 5 -> 4 -> 3 -> 0 -> 2
  *    snd  <- pop()      // 5 -> 4 -> 3 -> 0
  *    trd  <- pop()      // 5 -> 4 -> 3
  *    fth  <- pop()      // 5 -> 4
  *  } yield (fst, snd, trd, fth)
  *
  *  stack.run(List(3, 4, 5)) // (List(4, 5),(1, 2, 0, 3))
  * }}}
  */
case class State[S, +A](run: S => (S, A))
