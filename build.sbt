// (5) shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.{crossProject, CrossType}

val scala212Settings = Seq(
  scalaVersion := "2.12.4",
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

// Scala Native doesn't support 2.12 yet
val scala211Settings = Seq(
  scalaVersion := "2.11.12",
  scalacOptions ++= Seq("-optimize")
)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %%% "fansi"     % "0.2.5",
      "org.scalatest" %%% "scalatest" % "3.1.0-SNAP6" % Test
    )
  )
  .jvmSettings(scala212Settings)
  .jsSettings(scala212Settings)
  .jsSettings(
    scalaJSUseMainModuleInitializer := true
  )
  .nativeSettings(scala211Settings)
  .nativeSettings(test := {}) //tests don't work in native

lazy val coreJvm = core.jvm
lazy val coreJs  = core.js
lazy val coreNative  = core.native
