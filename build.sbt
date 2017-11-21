
lazy val deps = libraryDependencies := {
  val catsV       = "1.0.0-MF"
  val fs2V        = "0.10.0-M7"
  val scalaTestV  = "3.0.4"
  val scalaCheckV = "1.13.4"

  Seq(
    "org.typelevel"  %% "cats-core"  % catsV,
    "co.fs2"         %% "fs2-core"   % fs2V,
    "co.fs2"         %% "fs2-io"     % fs2V,
    "org.scalatest"  %% "scalatest"  % scalaTestV % "test",
    "org.scalacheck" %% "scalacheck" % scalaCheckV % "test"
  )
}

lazy val scalaOpts = scalacOptions := Seq(
  "-unchecked",
  "-feature",
  "-deprecation",
  "-encoding",
  "utf8",
  "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-nullary-override", // Warn when non-nullary overrides nullary, e.g. def foo() over def foo.
  "-Ypartial-unification",
  "-language:higherKinds",
  "-language:implicitConversions"
)

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/rsoeldner/gecko")),
  licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(ScmInfo(url("https://github.com/rsoeldner/gecko"), "scm:git:git@github.com:rsoeldner/gecko.git")),
  autoAPIMappings := true,
  apiURL := None,
  bintrayRepository := "gecko",
  pomExtra :=
    <developers>
      <developer>
        <id>jmcardon</id>
        <name>Jose Cardona</name>
        <url>https://github.com/jmcardon/</url>
      </developer>
      <developer>
        <id>rsoeldner</id>
        <name>Robert Soeldner</name>
        <url>https://github.com/rsoeldner/</url>
      </developer>
    </developers>
)

lazy val gecko = Project(id = "gecko", base = file("."))
  .settings(
    organization in ThisBuild := "io.github.rsoeldner",
    scalaVersion in ThisBuild := "2.12.4",
    version in ThisBuild := "0.0.1"
  )
  .settings(scalaOpts)
  .settings(deps)
  .settings(publishSettings)
