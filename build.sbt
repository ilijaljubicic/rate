name := "rate"

scalaVersion in ThisBuild:= "2.11.8"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" 
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies  ++= Seq(
  // Last stable release
  "org.scalanlp" %% "breeze" % "0.13"
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1", "-verbosity", "1")

