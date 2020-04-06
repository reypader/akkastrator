name := "akkastrator"

version := "0.1"

scalaVersion := "2.13.1"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.10.3"
libraryDependencies += "com.jayway.jsonpath" % "json-path" % "2.4.0"


libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % Test
libraryDependencies += "org.mockito" % "mockito-core" % "3.2.4" % Test