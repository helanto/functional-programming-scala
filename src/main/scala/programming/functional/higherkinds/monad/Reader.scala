package programming.functional.higherkinds.monad

/** == The Reader Monad ==
  * We described that the type constructor `Function1[C, *]` is an instance of the [[Monad]] type class.
  * Just like other monadic values that we've met so far, a function can also be considered a value
  * with a context. The context for functions is that the value is not present yet and that we have
  * to apply that function to something in order to get its result value.
  *
  * The function monad is also called the '''reader monad'''. All the functions read from a common
  * source or a common environment. The reader monad allows us to treat functions as values with a
  * context. We can act as if we already know what the functions will return. It does this by gluing
  * functions together into one function and then giving that function's parameter to all of the
  * functions that it was glued from. So if we have a lot of functions that are all just missing one
  * parameter and they'd eventually be applied to the same thing, we can use the reader monad to
  * sort of extract their future results and the `flatMap` implementation will make sure that it all
  * works out.
  *
  * Example:
  * {{{
  *  import programming.functional.higherkinds.Monad
  *  import programming.functional.higherkinds.Monad._
  *  import programming.functional.higherkinds.monad.Reader
  *
  *  implicit val intReader: Monad[Reader[Int, *]] = readerMonad[Int]
  *
  *  val f = for {
  *    two    <- intReader.point(2)
  *    five   <- intReader.point(5)
  *    plus2  <- Reader((c: Int) => c + two)
  *    times5 <- Reader((c: Int) => c * five)
  *    result <- intReader.point(plus2 + times5)
  *  } yield result
  *
  *  f.run(3) // (3 + 2) + (3 * 5) == 20
  * }}}
  */
case class Reader[C, A](run: C => A)
