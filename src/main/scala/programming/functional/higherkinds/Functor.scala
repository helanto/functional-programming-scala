package programming.functional.higherkinds

/** == Functor type class ==
  *
  * A type class for type constructors that can be mapped over.
  *
  * Using Haskell terminology, the `Functor` type class can be described as:
  * {{{
  * class Functor f where
  *   fmap :: (a -> b) -> f a -> f b
  * }}}
  *
  * The type of `fmap` is interesting. Most type classes operate on a concrete type. For instance,
  * the infamous `Show` type class:
  * {{{
  * class Show a where
  *   show :: a -> String
  *
  * > :type show
  * show :: (Show a) => a -> String
  * }}}
  *
  * But now `f` is not a concrete type, but a type constructor that takes a single type parameter.
  *
  * Instances of the `Functor` type class can be defined as:
  * {{{
  * implicit case object ListFunctor extends Functor[List] {
  *   def map[A, B](fa: List[A])(ab: A => B): List[B] = fa map ab
  * }
  *
  * instance Functor [] where
  *   fmap = map
  * }}}
  *
  * Notice how we didn't write `instance Functor [a] where` or `implicit case class ListFunctor[A] extends Functor[List[A]]`,
  * because `F[_]` has to be a type constructor that takes one type (`[a]` and `List[A]` are already concrete types).
  *
  * We can think of many type constructors that could be instances of the `Functor` type class.
  * One interesting case is `Either`; can this be made a functor ? The `Functor` type class wants a
  * type constructor that takes only one type parameter but `Either` takes two. The idea is to
  * partially apply `Either`, by feeding it only one parameter so that it has one free parameter.
  *
  * {{{
  *   def eitherFunctor[C]: Functor[Either[C, *]] = new Functor[Either[C, *]] {
  *     def map[A, B](fa: Either[C, A])(ab: A => B): Either[C, B] = ???
  *   }
  *   def eitherFunctor[C]: Functor[({ type λ[T] = Either[C, T] })#λ] = new Functor[({ type λ[T] = Either[C, T] })#λ] {
  *     def map[A, B](fa: Either[C, A])(ab: A => B): Either[C, B] = ???
  *   }
  *
  *   instance Functor Either a where
  *     fmap f (Right x) = Right (f x)
  *     fmap f (Left x) = Left x
  * }}}
  *
  * It is a good idea to think of `F[_]` as a container or box type, just don't take it too literally,
  * because for some functors the box analogy has to be stretched really thin to still hold some truth.
  * A more correct term for what a `Functor` is would be '''computational context'''.
  *
  * == Laws ==
  * For all instances `F[_]` of the the functor type class, the following should hold:
  *   - For all instances `F[A]`,
  * {{{
  *   fa.map(identity) == identity(fa) == fa
  * }}}
  *   - For all instances `F[A]` and pure functions `A => B` and `B => C`,
  * {{{
  *   fa.map(bc compose ab) == fa.map(ab).map(bc)
  * }}}
  *
  * The laws imply that there won't be anything other than mapping going on behind the scenes. For example,
  * the following does not obey to the functor laws:
  * {{{
  *   def map(fa: Option[Int])(ab: Int => String): Option[String] = fa match {
  *     case Some(num) => Some(ab(num))
  *     case None      => Some("")
  *   }
  * }}}
  * The problem is that it generates a `String` out of thin air, which cannot be possible in a generic `Functor` implementation.
  *
  * The problem can also exist in generic `Functor` implementations. Consider the following:
  * {{{
  *   sealed trait CounterOption[+T]
  *   case object CounterNone extends CounterOption[Nothing]
  *   case class CounterSome(counter: Int, t: T) extends CounterOption[T]
  *
  *   object CounterOption {
  *     implicit case object CounterOptionFunctor extends Functor[CounterOption] {
  *       def map(fa: CounterOption[A])(ab: A => B): CounterOption[B] = fa match {
  *         case CounterNone       => CounterNone
  *         case CounterSome(i, a) => CounterSome(i + 1, ab(a))
  *       }
  *     }
  *   }
  * }}}
  * This also does not obey the rules of `Functor` type class because `map` does more than mapping
  * the underlying value.
  */
trait Functor[F[_]] { self =>

  /** Applies a function from A to B to a value in context `F[A]`.
    * @param fa initial instance in context F[A].
    * @param ab a function from A to B.
    * @tparam A input type A.
    * @tparam B output type B.
    * @return a value in context `F[B]` by mapping elements inside the container.
    */
  def map[A, B](fa: F[A])(ab: A => B): F[B]

  /** Lifts a function into the context `F[_]`.
    *
    * Transforms a function from `A` to `B`, to a function from `F[A]` to `F[B]`.
    * This is called lifting, but it should come to no surprise that it is equivalent to map; we are
    * simply applying input parameters in reverse order.
    *
    * {{{
    *   def map[A, B](fa: F[A])(ab: A => B): F[B]
    *   def lift[A, B](ab: A => B)(fa: F[A]): F[B]
    *   def lift[A, B](ab: A => B): F[A] => F[B]
    * }}}
    *
    * You can think of `map` as either a function that takes a function and a functor and then maps
    * that function over the functor, or you can think of it as a function that takes a function and
    * lifts that function so that it operates on functors. Both views are correct.
    * @param ab a function from A to B.
    * @return a function that takes a value `A` in context `F` and returns `F[B]`.
    */
  def lift[A, B](ab: A => B): F[A] => F[B] = fa => map(fa)(ab)

  /** Composes functors `F` and `G` to get back a new functor `F[G[*]]` */
  def compose[G[_]: Functor]: Functor[λ[α => F[G[α]]]] = new Functor[λ[α => F[G[α]]]] {

    /** Applies a function from A to B to a value in context `F[A]`.
      *
      * @param fa initial instance in context F[A].
      * @param ab a function from A to B.
      * @tparam A input type A.
      * @tparam B output type B.
      * @return a value in context `F[B]` by mapping elements inside the container.
      */
    override def map[A, B](fa: F[G[A]])(ab: A => B): F[G[B]] =
      self.map(fa)(ga => Functor[G].map(ga)(a => ab(a)))
  }
}

object Functor {

  /** The summoner of the type class. A helper method for summoning an instance of [[Functor]]
    * from the implicit scope. Example:
    * {{{
    *   def method[F[_]: Functor]: Functor[F] = {
    *    // We need to materialize the implicit Functor[F] instance. We can either use the
    *    // famous implicitly keyword.
    *    implicitly[Functor[F]]
    *    // or directly using the summoner defined here.
    *    Functor[F]
    *  }
    * }}}
    * @param F the implicit functor.
    * @tparam C the type constructor of interest.
    * @return the materialized implicit parameter.
    */
  @inline def apply[C[_]](implicit F: Functor[C]): Functor[C] = F

  /** An instance of the type class for [[scala.List]] type constructor */
  implicit val listFunctor: Functor[List] = new Functor[List] {

    /** Applies a function from A to B to a value in context `F[A]`.
      * @param fa initial instance in context F[A].
      * @param ab a function from A to B.
      * @tparam A input type A.
      * @tparam B output type B.
      * @return a value in context `F[B]` by mapping elements inside the container.
      */
    override def map[A, B](fa: List[A])(ab: A => B): List[B] = fa.map(ab)
  }

  /** An instance of the type class for [[scala.Option]] type constructor */
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {

    /** Applies a function from A to B to a value in context `F[A]`.
      * @param fa initial instance in context F[A].
      * @param ab a function from A to B.
      * @tparam A input type A.
      * @tparam B output type B.
      * @return a value in context `F[B]` by mapping elements inside the container.
      */
    override def map[A, B](fa: Option[A])(ab: A => B): Option[B] = fa.map(ab)
  }

  /** An instance of the type class for Function1[T, *] type constructor */
  def function1Functor[T]: Functor[T => *] = new Functor[T => *] {

    /** Applies a function from A to B to a value in context `F[A]`.
      * @param fa initial instance in context F[A].
      * @param ab a function from A to B.
      * @tparam A input type A.
      * @tparam B output type B.
      * @return a value in context `F[B]` by mapping elements inside the container.
      */
    override def map[A, B](fa: T => A)(ab: A => B): T => B = ab compose fa
  }

  /** Composes two functors `F[_]` and `G[_]` into a third functor `F[G[_]]`. This is a generic
    * composition that works for all functors `F[_]` and `G[_]`.
    *
    * Suppose we have a value of `F[G[A]]` and a function `A => B`. We cannot map the function over
    * the value even when both `F[_]` and `G[_]` are functors; it simply does not type match.
    * Instead we can compose `F[_]` and `G[_]` into a new functor, so to map over the value.
    * @example {{{
    *   import programming.functional.higherkinds.Functor
    *   import programming.functional.higherkinds.Functor._
    *
    *   val v: List[Option[Int]] = List(Some(1), None, Some(2), Some(3))
    *   val f: Int => Int = _ + 1
    *
    *   // It would be nice if we could map f over v
    *   v.map(f)  // Does not type match.
    *
    *   implicit val listOptionFunctor: Functor[({ type λ[A] = List[Option[A]] })#λ] =
    *     Functor.compose[List, Option]
    *   v.map(f)  //  List(Some(2), None, Some(3), Some(4))
    * }}}
    *
    * @tparam F outer functor.
    * @tparam G inner functor.
    * @return the composite functor `F[G[*]]`.
    */
  def compose[F[_]: Functor, G[_]: Functor]: Functor[λ[α => F[G[α]]]] = Functor[F].compose[G]

  /** Helper methods for instances of Functor type class */
  implicit class FunctorHelpers[F[_]: Functor, A](fa: F[A]) {
    def map[B](ab: A => B): F[B] = Functor[F].map(fa)(ab)
  }
}
