name := "gecko"

version := "0.1"

val catsV = "1.0.0-MF"

scalaVersion := "2.12.3"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "com.github.ichoran" %% "thyme"     % "0.1.2-SNAPSHOT",
  "org.typelevel"      %% "cats-core" % catsV
)

dependsOn(ProjectRef(uri("git://github.com/amaizing/amaizing-saddle.git"), "saddle-core"))
