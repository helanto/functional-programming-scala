import sbt._

object Dependencies {

  /** A type representing the version of a dependency.
    * TODO handle beta or pre-release version numbers.
    * @param major the number corresponding to major version.
    * @param minor the number corresponding to minor version.
    * @param patch the number corresponding to patch version.
    */
  private case class Version(major: Int, minor: Int, patch: Int)

  private val versions = new {

    val compile = new {
      val cats: Version = Version(2, 4, 1)
    }
  }

  val dependencies: Seq[ModuleID] = Seq(
    "org.typelevel" %% "cats-core" % versions.compile.cats
  )

  /** Implicitly transform a Version to a String */
  implicit private def versionToStr(v: Version): String = s"${v.major}.${v.minor}.${v.patch}"
}
