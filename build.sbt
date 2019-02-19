import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "sexp4s",
    libraryDependencies ++= Seq(
      fastParse,
      shapeless,
      kittens,
      catsCore,
      apacheCommonsText,
      specs2 % Test
    ) ++ monocle
  )

