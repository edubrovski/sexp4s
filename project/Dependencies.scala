import sbt._

object Dependencies {

  lazy val specs2 = "org.specs2" %% "specs2-core" % "4.4.1"
  lazy val fastParse = "com.lihaoyi" %% "fastparse" % "2.1.0"
  lazy val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"
  lazy val catsCore = "org.typelevel" %% "cats-core" % "1.6.0"
  lazy val kittens = "org.typelevel" %% "kittens" % "1.2.0"
  lazy val apacheCommonsLang = "org.apache.commons" % "commons-lang3" % "3.8.1"
  lazy val apacheCommonsText = "org.apache.commons" % "commons-text" % "1.6"


  val monocleVersion = "1.5.0-cats"

  lazy val monocle = Seq(
    "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion,
    "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion
  )
}

