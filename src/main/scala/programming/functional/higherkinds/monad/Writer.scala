package programming.functional.higherkinds.monad

import programming.functional.{Id, Monoid}

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
  *   import programming.functional.higherkinds.monad.Writer._
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

object Writer {
  import programming.functional.higherkinds.Monad
  import programming.functional.higherkinds.Monad.MonadHelpers

  /** A monad transformer for `Writer` monad. Assuming that `F[_]` is a monad, returns a monad for `F[Writer[*]]` */
  def writerTMonad[F[_]: Monad, C: Monoid]: Monad[λ[α => F[Writer[C, α]]]] =
    new Monad[λ[α => F[Writer[C, α]]]] {

      /** Allows us to have a value in a context `F[A]` and then feed that into a function that takes
        * a normal value and returns a value in a new context `A => F[B]`.
        *
        * @param fa  a value in context F[A]
        * @param afb a function from A to a new value in context.
        * @return a value in context F[B]
        */
      override def flatMap[A, B](fa: F[Writer[C, A]])(afb: A => F[Writer[C, B]]): F[Writer[C, B]] =
        fa.flatMap { writer1 =>
          afb(writer1.value).map { writer2 =>
            Writer(Monoid[C].combine(writer1.context, writer2.context), writer2.value)
          }
        }

      /** Lifts a value of A into context `F[A]`.
        * A better way of thinking about `point` would be to say that it takes a value and puts it in some sort of default (or pure)
        * context; a minimal context that still yields that value.
        *
        * @param a input value of A.
        * @return the lifted value.
        */
      override def point[A](a: => A): F[Writer[C, A]] = Monad[F].point(Writer(Monoid[C].zero, a))
    }

  /** An instance of monad type class for `Writer[C, *]` type constructor */
  def writerMonad[C: Monoid]: Monad[λ[α => Writer[C, α]]] = Writer.writerTMonad[Id, C]
}
