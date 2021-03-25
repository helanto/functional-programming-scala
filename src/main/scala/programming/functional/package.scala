package programming

import programming.functional.higherkinds.Monad

package object functional {

  /** An alias to a fake monad. Encodes an alias to make identity instances well-kinded.
    *
    * The identity monad can be seen as the ambient monad that encodes the effect of having no
    * effect. It is ambient in the sense that plain pure values are values of `Id`.
    * @tparam A the enclosing type.
    */
  type Id[A] = A

  /** An instance of Monad, Functor and Applicative type class for [Id] data type */
  implicit case object IdExtensions extends Monad[Id] {

    /** Allows us to have a value in a context `F[A]` and then feed that into a function that takes
      * a normal value and returns a value in a new context `A => F[B]`.
      *
      * @param fa  a value in context F[A]
      * @param afb a function from A to a new value in context.
      * @return a value in context F[B]
      */
    override def flatMap[A, B](fa: Id[A])(afb: A => Id[B]): Id[B] = afb(fa)

    /** Lifts a value of A into context `F[A]`.
      * A better way of thinking about `point` would be to say that it takes a value and puts it in some sort of default (or pure)
      * context; a minimal context that still yields that value.
      *
      * @param a input value of A.
      * @return the lifted value.
      */
    override def point[A](a: => A): Id[A] = a
  }
}
