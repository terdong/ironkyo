ThisBuild / version          := "0.1.2"
ThisBuild / organization     := "com.github.terdong"
ThisBuild / scalaVersion     := "3.8.4"

lazy val root = project.in(file("."))
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
      "io.github.iltotore" %%% "iron"     % "3.3.1",
      "io.getkyo"          %%% "kyo-core" % "1.0.0-RC4",
      "org.scalameta"      %%% "munit"    % "1.3.3" % Test
    )
  )
