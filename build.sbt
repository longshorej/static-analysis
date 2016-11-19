name := "static analysis examples"

description := "https://www.youtube.com/watch?v=POvX4hYIoxg"

version := "0.1.0"

scalaVersion := "2.11.8"

organization := "info.longshore"

scalacOptions ++= Vector(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Yrangepos",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Ywarn-unused-import",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked"
)

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions in (Compile, console) ~= (_ filterNot (_ == "-Ywarn-unused-import"))

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.5" % "test")

initialCommands in console := """
  import info.longshore.staticanalysis._
  import Dsl._
"""
