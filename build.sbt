scalaVersion := "2.11.2"

libraryDependencies <+= scalaVersion { s =>
  "org.scala-lang" % "scala-reflect" % s
}

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.+" % "test"

