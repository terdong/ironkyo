ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.teamgehem"
ThisBuild / scalaVersion     := "3.8.4"

lazy val root = (project in file("."))
  .settings(
    name := "ironkyo",
    libraryDependencies ++= Seq(
      "io.github.iltotore" %% "iron"     % "3.3.1",
      "io.getkyo"          %% "kyo-core" % "1.0.0-RC2"
    )
  )
