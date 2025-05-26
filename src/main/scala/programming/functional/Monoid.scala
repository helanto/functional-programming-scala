package programming.functional

/** The monoid type class */
trait Monoid[A] {

  /** Combines or reduces two instances of A into a new A.
    *
    * @param a an instance of type A.
    * @param b an instance of type A.
    * @return the reduced result.
    */
  def combine(a: A, b: A): A

  /** The zero element of the monoid. It should adhere to the laws. */
  def zero: A
}

object Monoid {

  /** The summoner of the type class; a helper method for summoning an instance of Monoid[A]
    * from the implicit scope.
    * {{{
    *  def method[A: Monoid]: Monoid[A] = {
    *    // We need to materialize the implicit Monoid[A] instance. We can either use the
    *    // famous implicitly keyword.
    *    implicitly[Monoid[A]]
    *    // or directly using the summoner defined here.
    *    Monoid[A]
    *  }
    * }}}
    * @param monoid the implicit Monoid of A.
    * @return the materialized implicit parameter.
    */
  @inline def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid

  /** An implementation of the type class for [[java.lang.String]] type */
  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def combine(a: String, b: String): String = a + b

    override def zero: String = ""
  }

  /** An implementation of the type class for [[scala.Int]] type */
  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def combine(a: Int, b: Int): Int = a + b

    def zero: Int = 0
  }

  /** An implementation of the type class for [[scala.Boolean]] data type */
  implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(a: Boolean, b: Boolean): Boolean = a || b

    override def zero: Boolean = false
  }

  /** An implementation of the type class for List[A] data type */
  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def combine(a: List[A], b: List[A]): List[A] = a ++ b

    def zero: List[A] = List.empty[A]
  }

  /// Gives access to the Monoid type class methods using an OOP style.
  implicit class MonoidOps[A](val a: A) extends AnyVal {
    def combine(b: A)(implicit monoid: Monoid[A]): A = monoid.combine(a, b)

    def zero(implicit monoid: Monoid[A]): A = monoid.zero
  }
}
