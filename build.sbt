name := "Salesman"

version := "1.0"

scalaVersion := "2.10.4"

autoCompilerPlugins := true

libraryDependencies += compilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.4")

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.3"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.3.3"

scalacOptions += "-P:continuations:enable"
