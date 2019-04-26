scalacOptions += "-Ypartial-unification"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "com.github.mpilquist" %% "simulacrum" % "0.14.0",
  "org.typelevel" %% "cats-core" % "1.6.0"
  
)

scalaVersion := "2.12.8"
