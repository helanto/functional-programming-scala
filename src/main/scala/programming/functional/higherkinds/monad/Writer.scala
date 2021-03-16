package programming.functional.higherkinds.monad

import programming.functional.Monoid

/** == The Writer Monad ==
  * We have seen that all monads provide a way to lift a value of type `A` into a context `F[_]`,
  * and by using `flatMap` focus on the values themselves while the context gets handled for us.
  *
  * The [[Writer]] monad is for values that have another value attached that acts as a sort of log
  * value. We can perform computations on the values making sure that the attached values are combined
  * into larger log values which get attached to the result.
  *
  * @example {{{
  *   import programming.functional.higherkinds.Monad
  *   import programming.functional.higherkinds.Monad._
  *   import programming.functional.higherkinds.monad.Writer
  *
  *   def plus(x: Int)(value: Int): Writer[String, Int] = Writer(" plus " + x, x + value)
  *   def times(x: Int)(value: Int): Writer[String, Int] = Writer(" times " + x, x * value)
  *
  *   implicit val stringWriter: Monad[Writer[String, *]] = writerMonad[String]
  *
  *   for {
  *     one    <- Writer("one", 1)
  *     plus2  <- plus(2)(one)
  *     times3 <- times(3)(plus2)
  *   } yield times3  // Writer(one plus 2 times 3, 9)
  * }}}
  */
final case class Writer[C: Monoid, A](context: C, value: A) {
  def mapContext(f: C => C): Writer[C, A] = copy(context = f(context))
}
