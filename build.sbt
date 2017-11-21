name := "gecko"

version := "0.1"

val catsV = "1.0.0-MF"

scalaVersion := "2.12.3"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "org.typelevel"  %% "cats-core"  % catsV,
  "co.fs2"         %% "fs2-core"   % "0.10.0-M7",
  "co.fs2"         %% "fs2-io"     % "0.10.0-M7",
  "org.scalatest"  %% "scalatest"  % "3.0.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)

scalacOptions := Seq(
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
