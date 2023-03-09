val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "BFS",
    version := "0.0.1",

    scalaVersion := scala3Version,
    libraryDependencies += "org.jsoup" % "jsoup" % "1.14.3"
  )
