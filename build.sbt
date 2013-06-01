name := "FYT"

version := "1.0-SNAPSHOT"

organization := "net.tummi"

scalaVersion := "2.10.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9" % "test"

seq(ScctPlugin.instrumentSettings : _*)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
