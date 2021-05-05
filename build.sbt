lazy val commonSettings = Seq(
  organization := "edu.berkeley.cs",
  version := "1.1",
  scalaVersion := "2.11.12",
  libraryDependencies ++= Seq("com.github.scopt" %% "scopt" % "3.3.0"),
  libraryDependencies ++= Seq("com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3"),
  libraryDependencies ++= Seq("com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3")
)

lazy val torture = (project in file("."))
  .settings(commonSettings)
  .dependsOn(generator, testrun, overnight, fileop)

lazy val generator = (project in file("generator"))
  .settings(commonSettings)

lazy val testrun = (project in file("testrun"))
  .settings(commonSettings)
  .dependsOn(generator)

lazy val overnight = (project in file("overnight"))
  .settings(commonSettings)
  .dependsOn(testrun, fileop)

lazy val fileop = (project in file("fileop"))
  .settings(commonSettings)
