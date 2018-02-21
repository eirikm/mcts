val commonSettings = Seq(
  scalaVersion := "2.12.4",
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % "1.0.1",
    "com.lihaoyi"   %%% "fansi"     % "0.2.5",
    "org.scalatest" %%% "scalatest" % "3.0.4" % Test
  )
)

lazy val core = crossProject
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(commonSettings)
  .jsSettings(
    scalaJSUseMainModuleInitializer := true
  )
  .jvmSettings(
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-opt:l:inline",
        "-opt-inline-from:**",
        "-opt:closure-invocations",
        "-opt:copy-propagation",
        "-opt:box-unbox",
        "-opt:unreachable-code",
        "-opt:redundant-casts",
        "-opt-warnings"
      )
  )

lazy val coreJvm = core.jvm
lazy val coreJs  = core.js
