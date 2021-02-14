import Build._

// In Scala-3.x we do not need this plugin anymore. https://github.com/typelevel/kind-projector
addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.full)

ThisBuild / version := "0.0.1"

val projectName: String = "functional-programming-scala"

lazy val root: Project = (project in file("."))
  .settings(scalaVersion := versionScala)
  .settings(name := projectName)
  .settings(crossScalaVersions := Nil)
  .settings(libraryDependencies ++= Dependencies.dependencies)

// Format *.sbt and project/*.scala files, main sources and test sources
addCommandAlias("fmt-format", "all scalafmtSbt scalafmt test:scalafmt")
// Check *.sbt and project/*.scala files, main sources and test sources for formatting
addCommandAlias("fmt-check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
