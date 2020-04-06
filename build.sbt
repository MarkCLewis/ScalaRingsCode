fork := true
name := "ScalaRingsCode"
organization := "edu.trinity"
version := "0.1.0-SNAPSHOT"
javaHome := Some(file("/home/mlewis/graalvm-ee-19.0.2"))
scalacOptions := Seq("-unchecked", "-deprecation")
javaOptions := Seq("-Xmx32g")
scalaVersion := "2.12.11"
libraryDependencies += "org.scala-lang.modules" % "scala-swing_2.12" % "2.0.3"
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.192-R14"

