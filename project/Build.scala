import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    organization := "com.suguruhamazaki",
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.11.5",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xlint"),
    shellPrompt := { state => Project.extract(state).currentProject.id + " > " }
  )
}

object Dependencies {
  val akkaVersion = "2.3.9"

  val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val akkaSlf4j = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion
  val akkaTestkit = "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test"
  val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"
  val logback = "ch.qos.logback" % "logback-classic" % "1.1.2"
  val scalatest = "org.scalatest" %% "scalatest" % "2.2.1" % "test"
  val pegdown = "org.pegdown" % "pegdown" % "1.4.2" % "test"
  val mockito = "org.mockito" % "mockito-core" % "1.9.5" % "test"
  val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

  val commonDeps = Seq(
    akkaActor,
    akkaSlf4j,
    akkaTestkit,
    scalaLogging,
    logback,
    scalatest,
    pegdown,
    mockito,
    parserCombinators
  )
}

object TinyLispBuild extends Build {

  import BuildSettings.buildSettings
  import Dependencies.commonDeps
  import org.scalastyle.sbt.ScalastylePlugin.{ projectSettings ⇒ ScalastyleSettings }
  import ScctPlugin.instrumentSettings
  import com.typesafe.sbt.SbtScalariform.scalariformSettings

  lazy val root = Project("tiny-lisp",
			  file("."),
                          settings = buildSettings ++ instrumentSettings ++ ScalastyleSettings ++ scalariformSettings) settings (
                    libraryDependencies ++= commonDeps,
                    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/report", "-o"))
}
