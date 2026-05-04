name := "exampleScala2"

version := "0.0.1"

scalaVersion := "3.8.3"

resolvers ++= Resolver.sonatypeOssRepos("snapshots")

lazy val zioVersion = "2.1.25"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
  "dev.zio" %% "zio-test-junit" % zioVersion % Test,
  "dev.zio" %% "zio-test-magnolia" % zioVersion % Test
)

Compile / tpolecatScalacOptions ~= { opts =>
  // for simplicity in adts examples
  import org.typelevel.scalacoptions.ScalacOptions
  opts.filterNot(o => o == ScalacOptions.fatalWarnings || o == ScalacOptions.warnError)
}

scalacOptions ++= Seq(
  "-encoding", "UTF-8"
)
