package programming.functional.higherkinds

import programming.functional.higherkinds.monad.{Reader, State, Writer}
import programming.functional.Monoid

/** == Monad type class ==
  * Monad extends over applicative, concerning with the following: ''how do you apply a function of
  * type a -> m b to a value of type m a''. That is, how do you extract a value from the context
  * `F[_]` and pass it to a function that expects a normal value ?
  *
  * == Monad Transformers ==
  * Contrarily to functors and applicatives, '''monads do not compose''', at least not in a general
  * way. This last bit of information cannot be overlooked: we cannot magically and for free
  * compose any two monads together. This is not greatly surprising because we use monads to model
  * effects and effects don’t in general compose.
  *
  * While monads do not compose generally, you can manually derive the monad definition for most
  * composite monads. For example `List[Option[*]]` forms a monad; the same does happen for
  * `Future[IO[*]]`, and so on. When you start writing down your composite monad definitions you
  * start realizing that you do not need to know anything specific about the "outer" monad. Instead,
  * you only need some specific knowledge about the "inner" monad so to be able to compose the two.
  * To make it concrete:
  *   - Composing `List[Option[*]]` and `List[IO[*]]` cannot be generalized and two separate
  *   custom-written monad definitions are needed.
  *   - Composing `List[Option[*]]` and `Future[Option[*]]` can be generalized and achieved in a
  *   single monad definition.
  *
  * This generic composition of monads for each specific "inner" monad, is what monad transformers
  * is all about. With monad transformers we can stack many effects on top of a single data type,
  * and have it act as a single effect. We are saying we need a single effect that depicts the
  * multi-layered effect. Monad transformers allow us to squash together monads, creating one monad
  * where we previously had two or more. With this transformed monad we can avoid nested calls to `flatMap`.
  *
  * == Laws ==
  * Just like [[Applicative]], and [[Functor]] before it, [[Monad]] comes with a few laws that all
  * monad instances must abide by. Just because something is made an instance of the Monad type class
  * doesn't mean that it's a monad.
  *
  * For all instances `F[_]` of the the monad type class, the following should hold:
  *   - For all values of type `A` and pure functions `A => F[B]` (''left identity''),
  * {{{
  *   Monad[F].flatMap(Monad[F].pure(a))(afb) == afb(a)
  * }}}
  *   - For all values of type `F[A]` (''right identity''),
  * {{{
  *   Monad[F].flatMap(fa)(Monad[F].pure) == fa
  * }}}
  *   - For all values of type `F[A]` and pure functions `A => F[B]` and `B => F[C]` (''associativity''),
  * {{{
  *   fa.flatMap(afb).flatMap(bfc) == fa.flatMap(a => afb(a).flatMap(bfc))
  * }}}
  *
  * Associativity law states that when we have a chain of monadic function applications with `>>=`,
  * it shouldn't matter how they are nested.
  */
trait Monad[F[_]] extends Applicative[F] {

  /** Given a function from the contained type A to a new type B it returns a new F[B].
    *
    * @param fa initial instance of F[A].
    * @param ab the function from A to B.
    * @tparam A contained type A.
    * @tparam B contained type B.
    * @return a F[B] by mapping elements of type A to B.
    */
  override def map[A, B](fa: F[A])(ab: A => B): F[B] = flatMap(fa)(a => point(ab(a)))

  /** Given a value and a function inside a container, applies the function to the value.
    *
    * @param fa  a container containing a value of type A.
    * @param fab a container containing a function from A to B.
    * @return a container containing the result of applying the function to A.
    */
  override def <*>[A, B](fa: F[A])(fab: F[A => B]): F[B] = flatMap(fa) { a =>
    map(fab)(ab => ab(a))
  }

  /** Allows us to have a value in a context `F[A]` and then feed that into a function that takes
    * a normal value and returns a value in a new context `A => F[B]`.
    *
    * @param fa a value in context F[A]
    * @param afb a function from A to a new value in context.
    * @return a value in context F[B]
    */
  def flatMap[A, B](fa: F[A])(afb: A => F[B]): F[B]

  /** Equivalent to `flatMap`.
    *
    * @param fa a value in context F[A]
    * @param afb a function from A to a new value in context.
    * @return a value in context F[B]
    */
  @inline def >>=[A, B](fa: F[A])(afb: A => F[B]): F[B] = flatMap(fa)(afb)

  /** Equivalent to `flatMap`.
    *
    * @param fa a value in context F[A]
    * @param afb a function from A to a new value in context.
    * @return a value in context F[B]
    */
  @inline def bind[A, B](fa: F[A])(afb: A => F[B]): F[B] = flatMap(fa)(afb)

  /** Flattens a nested `F` of `F` structure into a single-layer `F` structure.
    *
    * Example:
    * {{{
    *   val writerofwriter: Writer[String, Writer[String, Int]] = Writer("outer", Writer("inner", 10))
    *
    *   writerMonad[String].join(writerofwriter) // Writer("outerinner", 10)
    * }}}
    * @param ffa the nested `F[F[A]]`
    * @return the flattened `F[A]`
    */
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
}

object Monad {

  /** The summoner of the type class. A helper method for summoning an instance of [[Monad]]
    * from the implicit scope. Example:
    * {{{
    *   def method[F[_]: Monad]: Monad[F] = {
    *    // We need to materialize the implicit Monad[F] instance. We can either use the
    *    // famous implicitly keyword.
    *    implicitly[Monad[F]]
    *    // or directly using the summoner defined here.
    *    Monad[F]
    *  }
    * }}}
    * @param F the implicit monad.
    * @tparam M the type constructor of interest.
    * @return the materialized implicit parameter.
    */
  @inline def apply[M[_]](implicit F: Monad[M]): Monad[M] = F

  /** An instance of the type class for Function1[T, *] type constructor
    * Example:
    * {{{
    *   implicit val intFunction1 = function1Monad[Int]
    *
    *   val functionMonad = for {
    *     two    <- intFunction1.point(2)
    *     plus10 <- (c: Int) => c + 10
    *     times2 <- (c: Int) => c * two
    *     square <- (c: Int) => c * c
    *   } yield plus10 + times2 + square
    *
    *   functionMonad(5) // (10 + 5) + (2 * 5) + (5 * 5) == 50
    * }}}
    */
  def function1Monad[T]: Monad[T => *] = new Monad[T => *] {

    /** Allows us to have a value in a context `F[A]` and then feed that into a function that takes
      * a normal value and returns a value in a new context `A => F[B]`.
      *
      * @param fa  a value in context F[A]
      * @param afb a function from A to a new value in context.
      * @return a value in context F[B]
      */
    override def flatMap[A, B](fa: T => A)(afb: A => (T => B)): T => B = t => afb(fa(t))(t)

    /** Lifts a value of A into context `F[A]`.
      *
      * @param a input value of A.
      * @return the lifted value.
      */
    override def point[A](a: => A): T => A = _ => a
  }

  /** An instance of the type class for `Writer[C, *]` type constructor */
  def writerMonad[C: Monoid]: Monad[λ[α => Writer[C, α]]] = new Monad[Writer[C, *]] {

    /** Allows us to have a value in a context `F[A]` and then feed that into a function that takes
      * a normal value and returns a value in a new context `A => F[B]`.
      *
      * @param fa  a value in context F[A]
      * @param afb a function from A to a new value in context.
      * @return a value in context F[B]
      */
    override def flatMap[A, B](fa: Writer[C, A])(afb: A => Writer[C, B]): Writer[C, B] =
      afb(fa.value).mapContext(context => Monoid[C].combine(fa.context, context))

    /** Lifts a value of A into context `F[A]`.
      *
      * @param a input value of A.
      * @return the lifted value.
      */
    override def point[A](a: => A): Writer[C, A] = Writer(Monoid[C].zero, a)
  }

  /** An instance of the type class for `Reader[C, *]` type constructor */
  def readerMonad[C]: Monad[({ type λ[A] = Reader[C, A] })#λ] = new Monad[Reader[C, *]] {

    /** Lifts a value of A into context `F[A]`.
      *
      * @param a input value of A.
      * @return the lifted value.
      */
    def point[A](a: => A): Reader[C, A] = Reader(c => a)

    /** Allows us to have a value in a context `F[A]` and then feed that into a function that takes
      * a normal value and returns a value in a new context `A => F[B]`.
      *
      * @param fa  a value in context F[A]
      * @param afb a function from A to a new value in context.
      * @return a value in context F[B]
      */
    def flatMap[A, B](fa: Reader[C, A])(afb: A => Reader[C, B]): Reader[C, B] = Reader { c =>
      afb(fa.run(c)).run(c)
    }
  }

  /** Extension methods for instances of [[Monad]] type class */
  implicit class MonadHelpers[F[_]: Monad, A](fa: F[A]) {
    def flatMap[B](afb: A => F[B]): F[B] = Monad[F].flatMap(fa)(afb)
    def map[B](ab: A => B): F[B] = Monad[F].map(fa)(ab)
    def zip[B](fb: F[B]): F[(A, B)] = Monad[F].prod(fa, fb)
    def <*>[B](fab: F[A => B]): F[B] = Monad[F].<*>(fa)(fab)
    def ap[B](fab: F[A => B]): F[B] = Monad[F].ap(fa)(fab)

    /** Flattens an `F[F[C]]` into an `F[C]`. How does this even work ?
      * It requires an implicit function from A in which case it is an F[C] to an F[B]. This is
      * simply the `identity` function, which can be found in [[scala.Predef]].
      *
      * Example:
      * {{{
      *   implicit val stringWriter = writerMonad[String]
      *   val writerofwriter: Writer[String, Writer[String, Int]] = Writer("outer", Writer("inner", 10))
      *
      *   writerofwriter.flatten // Writer("outerinner",10)
      * }}}
      *
      * @param toFB an implicit function from `F[C]` to `F[B]`.
      * @return a flattened `F[C]`
      */
    def flatten[B](implicit toFB: A => F[B]): F[B] = Monad[F].join(fa.map(toFB))
  }
}
