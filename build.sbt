name := "scala-mcts"

val commonSettings = Seq(
  scalaVersion := "2.12.4",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test
)

lazy val fromJs      = project.in(file("from-js")).settings(commonSettings)
lazy val fromHaskell = project.in(file("from-haskell")).settings(commonSettings)
lazy val fromScala   = project.in(file("from-scala")).settings(commonSettings)
