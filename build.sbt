ThisBuild / version := "0.1.2"
ThisBuild / organization := "com.github.terdong"
ThisBuild / scalaVersion := "3.8.4"

val ironVersion = "3.3.1"
val kyoVersion = "1.0.0-RC5"
val munitVersion = "1.3.3"

lazy val root = project
  .in(file("."))
  .aggregate(ironkyo.jvm, ironkyo.js)
  .settings(
    name := "ironkyo-root",
    publish / skip := true
  )

lazy val ironkyo = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    name := "ironkyo",
    libraryDependencies ++= Seq(
      "io.github.iltotore" %%% "iron" % ironVersion,
      "io.getkyo" %%% "kyo-core" % kyoVersion,
      "org.scalameta" %%% "munit" % munitVersion % Test
    )
  )
