package programming.functional.higherkinds.monad

import programming.functional.higherkinds.Monad._
import programming.functional.higherkinds.monad.Trampoline.{Cont, Done, More}

/** == The Trampoline Monad ==
  * The `Trampoline` data type representing ''tail recursive'' computations.
  *
  * The name “trampoline” may sound fancy or even intimidating at first, but the basic intuition is
  * pretty simple: instead of letting the JVM run a recursive function and push a new frame to the
  * call stack each time the recursion is performed, we rewrite the recursive function in a way that
  * we, rather than the JVM, have control of its execution. We manage to do that by creating data
  * structures that describe the recursion. Those data structures live on the heap and do not occupy
  * stack space. Eventually, ''at the end of the world'', the data structures are interpreted in a
  * stack-safe environment.
  *
  * Scala compiler is able to eliminate tail recursive function calls, but is limited to
  * self-recursive functions. Functions composed of smaller functions are prone to overflowing the
  * stack. Consider the mutual exclusion problem:
  * {{{
  *   def isEven(x: Int): Boolean = if (x == 0) true else isOdd(x - 1)
  *
  *   def isOdd(x: Int): Boolean =
  *    if (x == 0) false else isEven(x - 1)
  *
  *   isOdd(100000)  // Boom!
  * }}}
  * The recursive call appears in tail-call position but Scala compiler cannot optimize the
  * recursion because it is not self-recursive. The function will simply overflow the stack. Even
  * the calculation of factorial, the indisputable king of recursive functions, can lead to overflow
  * problems if not carefully crafted. In the following implementation, the recursive call does
  * not appear in a tail-call position; again a stack overflow will occur.
  * {{{
  *   def fact(n: Int): Long = n match {
  *     case 1 => 1L
  *     case n => fact(n-1) * n
  *   }
  *
  *   fact(500000) // Boom!
  * }}}
  *
  * Instead of letting the program control just flow through the recursive calls, we need to
  * suspend recursion by explicitly returning a value. The returned value will be a recipe to the
  * caller '''describing the remaining computations'''. It should come as no surprise that a recipe
  * is nothing more than a function which, when invoked, continues the computation. We can rewrite
  * our factorial implementation as in the following:
  * {{{
  *   // A description of a computation that needs to be executed: a “lazy” computation.
  *   // The function it wraps is only evaluated when we explicitly run it, which means that we gained control over recursion.
  *   case class Trampoline[+T](run: () => T)
  *
  *   def fact(n: Int): Trampoline[Long] = n match {
  *     case 1 => Trampoline(() => 1L)
  *     case n => Trampoline(() => fact(n-1).run() * n)
  *   }
  *
  *   val computation = fact(50000)  // Good!
  *   computation.run() // Bad!
  * }}}
  * In the previous example, a call to the `fact` function returns immediately with a concrete
  * value. This value represents the factorial computation. It looks like we've solved our problem.
  * Hmmm, not really... When we call `run()` we open Pandora's box.
  *
  * The problem is that we call `run` from within the description of our computation (inside
  * `Trampoline` thunk). We have to do so, because `fact(n-1)` returns a `Trampoline` which we wrap
  * inside another `Trampoline` so to achieve laziness. This would result to a `Trampoline[Trampoline[Long]]`
  * which does not type check. ''If only our `Trampoline` data type was a `Monad` ...''
  *
  * Before diving into monadic madness, can we find any workaround ? We can extend our description
  * of tail-recursive computations by adding a new data constructor. This constructor will contain
  * the description of a tail-recursive computation itself, without having to unwrap it first. There
  * is no magic involved here, we bake into our data type the case we couldn't handle before.
  * {{{
  *   sealed trait Trampoline[+T]
  *   // We not need laziness anymore in this data constructor, because it indicates a completed computation.
  *   case class Done[+T](result: T) extends Trampoline[T]
  *   case class More[+T](computation: () => Trampoline[T]) extends Trampoline[T]
  *
  *   // A tail-recursive stack-safe interpreter of the trampoline.
  *   def run[T](t: Trampoline[T]): T = t match {
  *     case Done(result)      => result
  *     case More(computation) => run(computation())
  *   }
  *
  *   def fac(n: Int): Trampoline[Long] = n match {
  *     case 1 => Done(1L)
  *     case n => More(() => fac(n-1))
  *   }
  *
  *   val computation = fac(50000)  // Good!
  *   run(computation)  // Good!
  * }}}
  * At the end of the world we ''interpret the computation'' in a stack-safe recursive function. Now
  * we are good! When the `run` is called we make sure we unwrap the computation in a stack-safe
  * environment. The only problem is that we did not really calculate factorial.
  *
  * Let's take a step back for a moment and consider what we achieved. We managed to perform a
  * recursive computation in a stack-safe way. And while our attempt is not enough to solve
  * factorial, it can solve the mutual recursion problem:
  * {{{
  *   def isEven(x: Int): Boolean =
  *    if (x == 0) true else isOdd(x - 1)
  *
  *   def isOdd(x: Int]): Boolean =
  *    if (x == 0) false else isEven(x - 1)
  *
  *   isOdd(100000)  // Stack unsafe
  *
  *   def isEvenT(x: Int): Trampoline[Boolean] =
  *    if (x == 0) Done(true) else More(() => isOddT(x - 1))
  *
  *   def isOddT(x: Int): Trampoline[Boolean] =
  *    if (x == 0) Done(false) else More(() => isEvenT(x - 1))
  *
  *   run(isOddT(100000)) // Stack safe
  * }}}
  *
  * The problem with factorial is that, without calling `run`, we cannot perform multiplication.
  * And if we call `run` from within factorial we no longer eliminate our recursive computation
  * inside the implementation of `run`. Instead, control flow will trampoline between `run` and
  * `fac` functions. If we write `case n => More(() => Done(run(fac(n-1)) * n))` we make a call to
  * `run` from a non-tail-call position.
  *
  * ''We really wish our `Trampoline` data type to be a `Functor`'', so we can `map` inside the
  * trampoline context. It turns out it is more than a functor; [[Trampoline]] is a
  * [[programming.functional.higherkinds.Monad Monad]]. You might be tempted to implement `flatMap`
  * as in the following:
  * {{{
  *   sealed trait Trampoline[+A] {
  *     def flatMap[B](atb: A => Trampoline[B]): Trampoline[B] = More[B](() => f(run(this)))
  *   }
  * }}}
  * But again, we are calling `run` from withing `flatMap`, passing recursively control flow between
  * the two functions. It seems that, no matter how hard we try, we are out of luck. The way around
  * this limitation is to add yet again a constructor to the trampoline data type, changing
  * `flatMap` from a method call to a constructor call. We just need to ''interpret'' `FlatMap` data
  * constructor appropriately in the `run` tail recursive function.
  *
  * This implementation is based on "Stackless Scala with Free Monads" [[http://blog.higher-order.com/assets/trampolines.pdf]]
  * and extends over [[scala.util.control.TailCalls.TailRec]].
  *
  * @example {{{
  *     import programming.functional.higherkinds.monad.Trampoline
  *     import programming.functional.higherkinds.monad.Trampoline._
  *     import programming.functional.higherkinds.Monad._
  *
  *     // Stack-unsafe fibonacci implementation.
  *     def fib(n: Int): Int = n match {
  *         case 0 => 0
  *         case 1 => 1
  *         case n => fib(n-1) + fib(n-2) // Not tail recursive
  *     }
  *
  *     // Stack-safe fibonacci implementation.
  *     def fibT(n: Int): Trampoline[Int] = n match {
  *         case 0 => done(0)
  *         case 1 => done(1)
  *         case n => tailcall(fibT(n-1)).flatMap(n1 => tailcall(fibT(n-2)).map(n2 => n1 + n2))
  *     }
  *
  *     // When you call `fibT(50)` nothing really happens; the `fibT` function immediately returns.
  *     // This is because `computation: => Trampoline[A]` is passed by-name in `tailcall` function.
  *     val fib50 = fibT(50)
  *     // When you `run` your Trampoline the recurssion kicks off. The fortunate thing is that
  *     // the `run` function is tail recursive.
  *     fib50.run
  * }}}
  */
sealed trait Trampoline[+T] {

  /** Runs the [[Trampoline]] and extracts the result. Call to run is ''stack safe''.
    *
    * @return the underlying value of the trampoline.
    */
  final def run: T = Trampoline.run[T](this)

  /** Feeds value from trampoline context into a function that takes a normal value and returns a
    * new value in the trampoline context `T => Trampoline[B]`.
    *
    * We might be tempted to implement `flatMap` like the following:
    * {{{
    *   def flatMap[A, B](fa: Trampoline[A])(afb: A => Trampoline[B]): Trampoline[B] =
    *     fa match {
    *        case Done(result)      => afb(result)
    *        case More(computation) => afb(computation().run)
    *      }
    * }}}
    *
    * However this implementation does not call `computation().run` in a tail recursive position.
    * Instead we are going to introduce a new data constructor on [[programming.functional.higherkinds.monad.Trampoline Trampoline]]
    * data type: `Cont`. By doing so, we exchange a method call (stack space) with a constructor
    * call (heap space).
    *
    * Now the implementation looks much simpler:
    * {{{
    *   def flatMap[A, B](fa: Trampoline[A])(afb: A => Trampoline[B]): Trampoline[B] =
    *     Cont(fa, afb)
    * }}}
    *
    * As long as we can interpret `Cont` data constructor correctly we should be good. Hmmm, not
    * quite! There is an edge case we need to consider. Suppose we have a recursive computation
    * represented with many nested `Cont` instances, as in the following:
    * {{{
    *   import programming.functional.higherkinds.monad.Trampoline._
    *   import programming.functional.higherkinds.Monad._
    *
    *   // computation is a 1000000 deep nested beast, but it is just data! We have not done any
    *   // recursion yet. It looks like the following:
    *   // computation => Cont(Cont(Cont(...), x => done(x + 1)), x => done(x + 1))
    *   val computation = List.fill(1000000)(1).foldLeft(done[Long](0L)) { case (t, n) =>
    *     t.flatMap(x => done(x + n))
    *   }
    * }}}
    *
    * We need to avoid building this left-leaning tower of `Cont` instances because, if not
    * interpreted properly, it might overflow the stack. We can disallow the construction of
    * deeply nested left-associated binds and always construct right-associated binds:
    * {{{
    *   import programming.functional.higherkinds.monad.Trampoline._
    *   import programming.functional.higherkinds.Monad._
    *
    *   def flatMap[A, B](fa: Trampoline[A])(afb: A => Trampoline[B]): Trampoline[B] =
    *     fa match {
    *       case c: Trampoline.Cont[a1, A] =>
    *         Trampoline.Cont[a1, B](c.sub, (x: a1) => c.atb(x) flatMap afb)
    *       case _                         => Trampoline.Cont(fa, afb)
    *     }
    *
    *   // computation is not any-more left-associated, but right associated. It looks like the following:
    *   // computation => Cont(Done(0L), x => done(x + 1).flatMap(x => done(x + 1).flatMap(...)) )
    *   val computation = List.fill(1000000)(1).foldLeft(done[Long](0L)) { case (t, n) =>
    *     t.flatMap(x => done(x + n))
    *   }
    * }}}
    *
    * @param afb a function from a normal value `T` to the trampoline context.
    * @return the result of applying a value of type `Trampoline[T]` to input function.
    */
  final def flatMap[B](afb: T => Trampoline[B]): Trampoline[B] = this match {
    case c: Cont[a1, T] => Cont[a1, B](c.sub, (x: a1) => c.atb(x) flatMap afb)
    case _              => Cont(this, afb)
  }
}

object Trampoline {

  /** Lifts a value of `T` into the trampoline context. It represents a function that has finished. */
  private case class Done[+T](result: T) extends Trampoline[T]

  /** Represents a tail call. It represents a function that has more work to do to produce a result.
    * You need to pass parameter `by-name` or lazily, just to describe the recursion.
    */
  private case class More[+T](computation: () => Trampoline[T]) extends Trampoline[T]

  /** The `Cont` data constructor let us extend or continue an existing computation by using the
    * result of the first computation to produce a second computation.
    *
    * It uses a [[https://en.wikipedia.org/wiki/Continuation-passing_style continuation passing style]];
    * the constructor takes an extra argument representing what should be done with the result the
    * first subroutine.
    */
  private case class Cont[A, +B](sub: Trampoline[A], atb: A => Trampoline[B]) extends Trampoline[B]

  /** Performs a tailcall. You need to wrap your recursive call into a tailcall.
    *  It is crucial to understand that your description of the computation is passed as
    * '''by-name''' parameter. This implies than when you do `tailcall(yourTrampoline)`, nothing
    * really happens. The actual computation will start when you call your `run` method. However
    * `run` method is tail recursive and thus ''stack safe''.
    *  @param rest  the expression to be evaluated in the tailcall. It is a
    *               [[https://docs.scala-lang.org/tour/by-name-parameters.html by-name]] parameter.
    *  @return a stack-safe [[Trampoline]] object representing the expression rest.
    */
  def tailcall[A](rest: => Trampoline[A]): Trampoline[A] = More(() => rest)

  /** Used to return final result from tail-calling computation.
    *  @param  result the result value
    *  @return a stack-safe [[Trampoline]] object representing a computation which immediately
    *          returns result
    */
  def done[A](result: A): Trampoline[A] = Done(result)

  @scala.annotation.tailrec
  private def run[T](tramp: Trampoline[T]): T = tramp match {
    case Done(result)      => result
    case More(computation) => run(computation())
    case c: Cont[b, T]     =>
      c.sub match {
        case Done(result)      => run(c.atb(result))
        case More(computation) => run(computation().flatMap(c.atb))
        case _c: Cont[a, b]    => run(_c.sub.flatMap((a: a) => _c.atb(a).flatMap(c.atb)))
      }
  }
}
