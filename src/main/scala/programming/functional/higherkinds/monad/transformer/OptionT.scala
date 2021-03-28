package programming.functional.higherkinds.monad.transformer

import programming.functional.higherkinds.{Applicative, Functor, Monad}
import programming.functional.Id

/** A '''monad transformer''' for [[scala.Option Option]] data type.
  *
  * `OptionT[F[_], A]` is a light wrapper on an `F[Option[A]]` with some convenient methods for
  * working with this nested structure.
  *
  * One significant improvement of using `OptionT` data type is extension methods. `Option` data
  * type exposes methods that would be nice have in `F[Option[T]]` data type. Examples of such
  * methods are `filter`, `fold`, `orElse` and more, that work directly with the underlying `Option`.
  *
  * However extension methods on their own, do not justify the existence of a wrapper data type. We
  * could simply have an `implicit class` instead. In the following section, we will try to get a
  * better understanding on why working with `OptionT` data type can be convenient.
  *
  * == Rational ==
  * Why do we need `OptionT` data type in the first place ? The short answer is that we do not
  * really need it. We can define an instance of the `Monad` type class for the monad transformer,
  * and for the most part we can work directly with `map`, `flatMap`, `zip` and `<*>` methods
  * provided by the monad definition. An indicative example of such a monad definition could be the
  * following:
  * {{{
  *   import programming.functional.higherkinds.Monad
  *   import programming.functional.Id
  *
  *   // Notice that Monad[λ[α => F[Option[α]]]] is different than Monad[F[Option[*]]
  *   def optionTMonad[F[_]: Monad]: Monad[λ[α => F[Option[α]]]] = new Monad[Lambda[α => F[Option[α]]]] {
  *     override def flatMap[A, B](fa: F[Option[A]])(afb: A => F[Option[B]]): F[Option[B]] =
  *       fa.flatMap {
  *         case Some(a) => afb(a)
  *         case None    => Monad[F].pure(None)
  *       }
  *     override def point[A](a: => A): F[Option[A]] = Monad[F].pure(Some(a))
  *   }
  *
  *   implicit val optionMonad: Monad[Option] = optionTMonad[Id]
  * }}}
  *
  * Let's put the monad transformer we created into use, by creating a concrete monad and start
  * ''flatMap''ing over it:
  * {{{
  *   import programming.functional.higherkinds.Monad
  *
  *   implicit val listOptionMonad: Monad[λ[α => List[Option[α]]]] = optionTMonad[List](Monad.listMonad)
  *
  *   val opt: Option[Int] = Some(5)
  *   val f: Int => List[Option[Int]] = x => List.fill(x)(Some(x))
  *
  *   val listOfOpt = OptionT.fromOption[List, Int](opt)
  *
  *   // Does not compile!
  *   // It expects a `Option[Int] => List[B]` and we pass a `Int => List[Option[B]]`
  *   val res = listOfOpt.flatMap(f)
  *
  *   // Compiles! But ugly!
  *   val res = MonadHelpers[λ[α => List[Option[α]]], Int](listOfOpt)
  *     .flatMap(f)  // List(Some(5), Some(5), Some(5), Some(5), Some(5))
  * }}}
  * An important issue arises; the compiler tries to find a method called `flatMap` on the "outer"
  * type constructor, in our case `List[_]`. Such method exists with a different type signature than
  * the type parameter we pass; __the code does not compile__. The problem is not that [[scala.List List]]
  * has a method named `flatMap`. Even if we change the name of the method, the code still does not
  * compile. Instead we need to somehow hint to the compiler that we want to treat `List[Option[_]]`
  * as a single type constructor `F[_]`. We can find other workarounds, e.g. a type alias
  * `type LO[A] = List[Option[A]]`, but none of them is elegant enough.
  *
  * Having a flat data structure that describes a multi-layered structure of monads is convenient.
  * It is easier to work with `OptionT[F, T]` than having to work with `F[Option[T]]`. In the first
  * case when we map over the data structure there is not ambiguity involved. We have to pass a
  * `T => B`. In the second case however, we have two options: We can either pass a `Option[T] => B`
  * or an `T => B`, depending on what we consider to be our functor, `F[_]` or `F[Option[_]]`. The
  * more monad layers the structure has, the more the alternatives.
  *
  * == Avoiding allocation overhead ==
  * Why we do not require `F[_]` to be a [[programming.functional.higherkinds.Monad Monad]]
  * itself ? For performance reasons. We want to treat `OptionT` as a [[https://docs.scala-lang.org/overviews/core/value-classes.html value class]].
  * If we add monad requirement for `F[_]`, we implicitly introduce a second constructor argument
  * and ''JVM'' cannot eliminate the wrapper class at runtime. Instead an instance allocation is
  * required.
  *
  * @example {{{
  *   import programming.functional.higherkinds.monad.transformer.OptionT
  *   import programming.functional.higherkinds.Monad
  *   import programming.functional.higherkinds.Monad._
  *
  *   implicit val listOptionMonad: programming.functional.higherkinds.Monad[OptionT[List, *]] =
  *     OptionT.optionTMonad[List](listMonad)
  *
  *   for {
  *     n1  <- OptionT.fromOption[List, Int](Some(2))
  *     n2  <- OptionT(List(Some(1), None, Some(-1)))
  *     sum <- listOptionMonad.pure(n1 + n2)
  *   } yield sum   // List(Some(3), None, Some(1)
  * }}}
  *
  * @note We cannot write `Monad[F[Option[*]]]` and expect the same behaviour as in `Monad[λ[α => F[Option[α]]]]`.
  *       The reason is that [[https://github.com/typelevel/kind-projector#type-lambda-gotchas kind-projector]] always binds `*` at the
  *       tightest level, transforming `F[Option[*]]` to `F[λ[α => Option[α]]]` which is different
  *       than `λ[α => F[Option[α]]]`.
  *
  * @param base the underlying `F[Option[T]]` that the class provides extensions for.
  * @tparam F the external "wrapper" type constructor.
  * @tparam T the internal data type.
  */
final case class OptionT[F[_], T](base: F[Option[T]]) extends AnyVal {

  def filterT(f: T => Boolean)(implicit F: Functor[F]): OptionT[F, T] =
    OptionT(F.map(base)(_.filter(f)))

  def getOrElseT[B >: T](default: => B)(implicit F: Functor[F]): F[B] =
    F.map(base)(_.getOrElse(default))

  def orElseT[B >: T](default: => Option[B])(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(base)(_.orElse(default)))

  def fold[B](ifEmpty: => B)(f: T => B)(implicit F: Functor[F]): F[B] =
    F.map(base)(_.fold(ifEmpty)(f))

}

object OptionT {

  /** Lifts `Option[A]` into the context of applicative `F` */
  def fromOption[F[_]: Applicative, A](optA: Option[A]): OptionT[F, A] = OptionT(
    Applicative[F].point(optA)
  )

  /** Lifts `F[A]` into the context of `F[Option[A]]` */
  def liftF[F[_]: Functor, A](fa: F[A]): OptionT[F, A] = OptionT(Functor[F].map(fa)(Option(_)))

  def optionTMonad[F[_]: Monad]: Monad[OptionT[F, *]] = new Monad[OptionT[F, *]] {

    /** Allows us to have a value in a context `F[A]` and then feed that into a function that takes
      * a normal value and returns a value in a new context `A => F[B]`.
      *
      * @param fa  a value in context F[A]
      * @param afb a function from A to a new value in context.
      * @return a value in context F[B]
      */
    override def flatMap[A, B](fa: OptionT[F, A])(afb: A => OptionT[F, B]): OptionT[F, B] = OptionT(
      Monad[F].flatMap(fa.base) {
        case Some(value) => afb(value).base
        case None        => Monad[F].point(None)
      }
    )

    /** Lifts a value of A into context `F[A]`.
      * A better way of thinking about `point` would be to say that it takes a value and puts it in some sort of default (or pure)
      * context; a minimal context that still yields that value.
      *
      * @param a input value of A.
      * @return the lifted value.
      */
    override def point[A](a: => A): OptionT[F, A] = OptionT(Monad[F].point(Option(a)))
  }

  implicit case object OptionMonad extends Monad[Option] {
    private val optionTMonad: Monad[OptionT[Id, *]] = OptionT.optionTMonad[Id]

    /** Allows us to have a value in a context `F[A]` and then feed that into a function that takes
      * a normal value and returns a value in a new context `A => F[B]`.
      *
      * @param fa  a value in context F[A]
      * @param afb a function from A to a new value in context.
      * @return a value in context F[B]
      */
    override def flatMap[A, B](fa: Option[A])(afb: A => Option[B]): Option[B] =
      optionTMonad.flatMap(OptionT[Id, A](fa))(afb andThen OptionT.apply[Id, B]).base

    /** Lifts a value of A into context `F[A]`.
      * A better way of thinking about `point` would be to say that it takes a value and puts it in some sort of default (or pure)
      * context; a minimal context that still yields that value.
      *
      * @param a input value of A.
      * @return the lifted value.
      */
    override def point[A](a: => A): Option[A] = optionTMonad.pure[A](a).base
  }
}
