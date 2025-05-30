# functional-programming-scala
[![Scala CI](https://github.com/helanto/functional-programming-scala/actions/workflows/scala.yml/badge.svg)](https://github.com/helanto/functional-programming-scala/actions/workflows/scala.yml)

## Using this project.
You need sbt installed to run this project. At the time of writing, `sbt` uses Java versions 8 or 11.
To run the project download the sbt from sources, and store under `/opt`:
```bash
$ sudo ln -sf /opt/sbt-v1.11.0/bin/sbt /usr/local/bin/sbt
$ vim /usr/local/bin/sbt
#!/usr/bin/env bash
# Manually edited
export JAVA_HOME=/Library/Java/JavaVirtualMachines/amazon-corretto-11.jdk/Contents/Home
```

Time to run the project:
```bash
$ sbt console
scala > import programming.functional.Monoid
import programming.functional.Monoid

scala> import programming.functional.Monoid._
import programming.functional.Monoid._

scala> Monoid[String].zero.combine("Hello")
val res0: String = Hello

scala> 8.combine(2)
val res1: Int = 10

scala> false.combine(true)
val res2: Boolean = true
```

## A story about kinds
**Type constructors** such as `List` or `Option` or `DStream` take other types as parameters to eventually produce concrete types. This reminds us of **functions** or **value constructors**, which take values as parameters to produce new values. Similarities do not end here; type constructors can be partially applied in the same fashion that functions can:
```scala
// Partially apply a value constructor.
val func: String => Int => String = ???
val funcA: Int => String = func("A")

// Partially apply a type constructor.
trait Either[L, R] { ??? }
type StringOrA[A] = Either[String, A]
```

But what types are ? They are little labels that values carry so that we can reason about them. Types themselves have their own labels called **kinds**. A kind is more or less the type of a type.
```
scala> :k -v String
String's kind is A
*
This is a proper type.

scala> :k -v Function2
Function2's kind is F[-A1,-A2,+A3]
* -(-)-> * -(-)-> * -(+)-> *
This is a type constructor: a 1st-order-kinded type.

scala> :k -v Monad
Monad's kind is X[F[A]]
(* -> *) -> *
This is a type constructor that takes type constructor(s): a higher-kinded type.
```

A star `*` stands for a type that does not take any type parameters: a concrete type. The only type that can be assigned to a value is a concrete type.

```scala
// Valid
val x: String = ???

// Invalid
val x: Function2 = ???

// Invalid
val x: Monad = ???
```
Now, `Function2` type constructor is more interesting; ignoring variance annotations it has kind `* -> * -> * -> *`. It takes as input a concrete type `*` and returns a type constructor that takes two type parameters and returns a concrete type `* -> * -> *`. Put differently, it takes as input three type parameters and returns a concrete type. `Monad` takes as input a type of kind `* -> *` and returns a concrete type. We cannot pass a `Function2` to a `Monad`, because the kinds do not match. Instead we can pass `Option` type constructor which has kind `* -> *`.

## References
- [[1](http://adriaanm.github.io/files/higher.pdf)] - Generics of a higher kind.