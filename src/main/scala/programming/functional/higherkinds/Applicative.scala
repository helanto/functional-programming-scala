package programming.functional.higherkinds

/** == Applicative type class ==
  *
  * Currying is one of the fundamentals characteristics of functional programming.
  * A function a -> b -> c can be considered as either a functions that takes two arguments a and b and returns a c,
  * or as a function that takes a single argument a and returns a function from b to c.
  *
  * What do we get if we partially apply a function to a functor ?
  * {{{
  *   def partial[F[_]: Functor, A, B, C](fa: F[A], f: A => B => C): F[B => C] = Functor[F].map(fa)(f)
  * }}}
  *
  * We see how by mapping "multi-parameter" functions over functors, we get functors that contain
  * functions inside them. So now what can we do with them ?
  *
  * This `F[B => C]` is the tricky part; Usually we need to apply the contained function to an `F[B]` rather than a `B`.
  * With normal functors, we're out of luck, because all they support is just mapping normal functions over existing functors.
  *
  * This exactly is the context that applicative type class comes into play.
  * It is more powerful than functor, but less powerful than monad.
  *
  * == Laws ==
  * For all instances `F[_]` of the the applicative type class, the following should hold:
  *   - For all instances `F[A]` and pure function `A => B`,
  * {{{
  *   Applicative[F].map(fa)(ab) == Applicative[F].<*>(fa)(Applicative[F].pure(ab))
  * }}}
  *   - For all instances `F[A]`,
  * {{{
  *   Applicative[F].<*>(fa)(Applicative[F].pure(identity)) == fa
  * }}}
  *   - For all instances `F[A]`, `F[B]` and `F[C]`,
  * {{{
  *   fa.zip(fb).zip(fc) == fa.zip(fb.zip(fc)) // Up to isomorphism
  * }}}
  */
trait Applicative[F[_]] extends Functor[F] { self =>

  /** Lifts a value of A into context `F[A]`.
    * A better way of thinking about `point` would be to say that it takes a value and puts it in some sort of default (or pure)
    * context; a minimal context that still yields that value.
    *
    * @param a input value of A.
    * @return the lifted value.
    */
  def point[A](a: => A): F[A]

  /** Equivalent to point; lifts a value into the container type.
    *
    * @param a input value of A.
    * @return the lifted value.
    */
  @inline def pure[A](a: => A): F[A] = point(a)

  /** Returns an `F[Unit]` value.
    *
    * It can be used as:
    * {{{
    *    val unit = Applicative[Option].unit
    * }}}
    *
    * @return
    */
  def unit: F[Unit] = pure(())

  /** Given a value and a function in a context `F`, applies the function to the value.
    *
    * @param fa a value in the context `F[A]`
    * @param fab a function in the context `F[A => B]`
    * @return a container with the result of applying the function to value inside the context.
    */
  def <*>[A, B](fa: F[A])(fab: F[A => B]): F[B]

  /** Equivalent to `Applicative[F].<*>`.
    *
    * The `ap` function is the same as `<*>` but in the [[programming.functional.higherkinds.Monad Monad]]
    * context. In ''Haskell'' programming language, the monad type class was introduced way before
    * applicative type class, hence monads do not have an applicative constraint. Put differently,
    * in ''Haskell'' monads are not applicatives. As a consequence ''Haskell'' uses the function
    * `ap` when wants to apply `<*>` in the context of monads.
    *
    * @param fa a value in the context `F[A]`
    * @param fab a function in the context `F[A => B]`
    * @return a container with the result of applying the function to value inside the context.
    */
  @inline def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = <*>(fa)(fab)

  /** Merges a pair of two values in the context `F`, into a single pair value in the same context.
    *
    * @param fa a value in the context `F[A]`
    * @param fb a value in the context `F[B]`
    * @return a value in the same context containing a pair `F[(A, B)]`
    */
  def prod[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    <*>(fa)(map(fb)(b => (a: A) => (a, b)))

  /** Equivalent to `Applicative[F].prod`.
    *
    * Zips two values in the context F, into a single pair value in the same context.
    *
    * @param fa a value in the context `F[A]`
    * @param fb a value in the context `F[B]`
    * @return a value in the same context containing a pair `F[(A, B)]`
    */
  @inline def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = prod(fa, fb)

  /** Applies a function from A to B to a value in context `F[A]`.
    *
    * @param fa initial instance in context F[A].
    * @param ab a function from A to B.
    * @tparam A input type A.
    * @tparam B output type B.
    * @return a value in context `F[B]` by mapping elements inside the container.
    */
  override def map[A, B](fa: F[A])(ab: A => B): F[B] = <*>(fa)(pure(ab))

  /** Composes applicatives `F` and `G` to get back a new applicative `F[G[*]]` */
  def compose[G[_]: Applicative]: Applicative[λ[α => F[G[α]]]] = new Applicative[λ[α => F[G[α]]]] {
    def point[A](a: => A): F[G[A]] = self.point(Applicative[G].point(a))

    def <*>[A, B](fa: F[G[A]])(fab: F[G[A => B]]): F[G[B]] =
      self.map(self.prod(fa, fab))(pair => Applicative[G].<*>(pair._1)(pair._2))
  }

  /** Transforms a [[scala.List List]] of [[programming.functional.higherkinds.Applicative Applicative]] into an
    * [[programming.functional.higherkinds.Applicative Applicative]] of [[scala.List List]].
    *
    * If we want to turn an empty list into an applicative with a list of results, we just put an
    * empty list in a default context.
    *
    * @param lfa a list of applicatives.
    * @return an applicative of lists.
    */
  def sequence[A](lfa: List[F[A]]): F[List[A]] = lfa match {
    case Nil          => pure(List.empty[A])
    case head :: next => <*>(sequence(next))(map(head)(h => ls => h :: ls))
  }
}

object Applicative {

  /** The summoner of the type class. A helper method for summoning an instance of [[Applicative]] from implicit scope.
    * Example:
    * {{{
    *   def method[F[_]: Applicative]: Applicative[F] = {
    *    // We need to materialize the implicit Applicative[F] instance. We can either use the
    *    // famous implicitly keyword.
    *    implicitly[Applicative[F]]
    *    // or directly using the summoner defined here.
    *    Applicative[F]
    *  }
    * }}}
    * @param A the implicit applicative.
    * @tparam F the type constructor of interest.
    * @return the materialized implicit parameter.
    */
  @inline def apply[F[_]](implicit A: Applicative[F]): Applicative[F] = A

  /** An instance of the type class for [[scala.Option]] type constructor */
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {

    /** Lifts a value of A into context `F[A]`.
      *
      * @param a input value of A.
      * @return the lifted value.
      */
    override def point[A](a: => A): Option[A] = Option(a)

    /** Given a value and a function in a context `F`, applies the function to the value.
      *
      * @param fa  a value in the context `F[A]`
      * @param fab a function in the context `F[A => B]`
      * @return a container with the result of applying the function to value inside the context.
      */
    override def <*>[A, B](fa: Option[A])(fab: Option[A => B]): Option[B] = fa match {
      case Some(a) => fab.map(ab => ab(a))
      case None    => None
    }
  }

  /** An instance of the type class for [[scala.List List]] type constructor.
    *
    * In this instance of the type class we perform a point-to-point join operation. The first
    * function in the right list gets applied to the first value in the left one, the second
    * function gets applied to the second value, and so on.
    *
    * This is not a true applicative since it does not satisfy the following applicative law:
    *   - For all instances `F[A]`,
    * {{{
    *   Applicative[F].<*>(fa)(Applicative[F].pure(identity)) == fa
    * }}}
    *
    * As an example, consider the case of `fa` being a `List(1, 2, 3)`:
    * {{{
    *   Applicative[List].<*>(List(1, 2, 3))(Applicative[List].pure(identity)) ==
    *   Applicative[List].<*>(List(1, 2, 3))(List(identity)) ==
    *   List(1) != List(1, 2, 3)
    * }}}
    *
    * The problem is that pure for a zip list should return an infinite stream of A's, which needs a
    * lazy data structure to represent.
    *
    * @example {{{
    *     val fa: List[Int] = 1 :: 2 :: Nil
    *     val fab: List[Int => Int] = ((i: Int) => i + 1) :: ((i: Int) => i * 2) :: Nil
    *     listZipApplicative.<*>(fa)(fab) == 2 :: 4 :: Nil
    * }}}
    *
    * @note `Haskell` implementation creates an infinite list of A's. And actually this satisfies
    *       all applicative laws, since we perform a point to point join operation.
    */
  val listZipApplicative: Applicative[List] = new Applicative[List] {

    /** Lifts a value of A into context `F[A]`.
      *
      * @param a input value of A.
      * @return the lifted value.
      */
    override def point[A](a: => A): List[A] = a :: Nil

    /** Given a value and a function in a context `F`, applies the function to the value.
      *
      * @param fa  a value in the context `F[A]`
      * @param fab a function in the context `F[A => B]`
      * @return a container with the result of applying the function to value inside the context.
      */
    override def <*>[A, B](fa: List[A])(fab: List[A => B]): List[B] = fa match {
      case a :: nextA =>
        fab match {
          case ab :: nextAB => ab(a) :: <*>(nextA)(nextAB)
          case Nil          => Nil
        }
      case Nil        => Nil
    }
  }

  /** An instance of the type class for [[scala.List]] type constructor.
    * It turns out there are actually more ways for lists to be applicative functors.
    *
    * In this instance of the type class we perform a cross join operation. For example:
    * {{{
    *     val fa: List[Int] = 1 :: 2 :: Nil
    *     val fab: List[Int => Int] = ((i: Int) => i + 1) :: ((i: Int) => i * 2) :: Nil
    *     listCrossApplicative.<*>(fa)(fab) == 2 :: 2 :: 3 :: 4 :: Nil
    * }}}
    */
  implicit val listCrossApplicative: Applicative[List] = new Applicative[List] {

    /** Lifts a value of A into context `F[A]`.
      *
      * @param a input value of A.
      * @return the lifted value.
      */
    override def point[A](a: => A): List[A] = a :: Nil

    /** Given a value and a function in a context `F`, applies the function to the value.
      *
      * @param fa  a value in the context `F[A]`
      * @param fab a function in the context `F[A => B]`
      * @return a container with the result of applying the function to value inside the context.
      */
    override def <*>[A, B](fa: List[A])(fab: List[A => B]): List[B] = fa match {
      case Nil        => Nil
      case a :: nextA => fab.map(ab => ab(a)) ++ <*>(nextA)(fab)
    }

  }

  /** An instance of the type class for Function1[T, *] type constructor */
  def function1Applicative[T]: Applicative[T => *] = new Applicative[T => *] {

    /** Lifts a value of A into context `F[A]`.
      *
      * @param a input value of A.
      * @return the lifted value.
      */
    override def point[A](a: => A): T => A = _ => a

    /** Given a value and a function in a context `F`, applies the function to the value.
      *
      * @param fa  a value in the context `F[A]`
      * @param fab a function in the context `F[A => B]`
      * @return a container with the result of applying the function to value inside the context.
      */
    override def <*>[A, B](fa: T => A)(fab: T => (A => B)): T => B = t => fab(t)(fa(t))
  }

  /** Composes two applicatives `F[_]` and `G[_]` into a third applicative `F[G[_]]`. This is a
    * generic composition that works for all applicatives `F[_]` and `G[_]`.
    *
    * Suppose we have a value of `F[G[A]]` and a function `F[G[A => B]]`. We cannot apply the
    * function over the value even when both `F[_]` and `G[_]` are applicatives; it simply does not
    * type match. Instead we can compose `F[_]` and `G[_]` into a new applicative, so to apply the
    * function.
    * @example {{{
    *   import programming.functional.higherkinds.Applicative
    *   import programming.functional.higherkinds.Applicative._
    *
    *   type LO[A] = List[Option[A]]
    *   val v: LO[Int] = List(Some(1), None, Some(2), Some(3))
    *   val f: LO[Int => Int] = List(Some(_ + 1), Some(_ + 2), None, Some(_ + 3))
    *
    *   implicit val listOptionApplicative =
    *     Applicative.compose[List, Option](listZipApplicative, optionApplicative)
    *
    *   // You would expect to return List(Some(2), None, None, Some(6)) !!
    *   // The problem is that point of listZipApplicative implementation is slightly wrong.
    *   // Instead of returning a lazy infinite list of A, it just returns a single A.
    *   v.ap(f).foreach(println)  // List(Some(2))
    * }}}
    *
    * @tparam F outer applicative.
    * @tparam G inner applicative.
    * @return the composite applicative `F[G[*]]`.
    */
  def compose[F[_]: Applicative, G[_]: Applicative]: Applicative[λ[α => F[G[α]]]] =
    Applicative[F].compose[G]

  /** Helper extension methods for [[Applicative]] type class */
  implicit class ApplicativeHelpers[F[_]: Applicative, A](fa: F[A]) {
    def map[B](ab: A => B): F[B] = Functor[F].map(fa)(ab)
    def zip[B](fb: F[B]): F[(A, B)] = Applicative[F].zip(fa, fb)
    def <*>[B](fab: F[A => B]): F[B] = Applicative[F].<*>(fa)(fab)
    @inline def ap[B](fab: F[A => B]): F[B] = <*>(fab)
  }
}
