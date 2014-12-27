name := "spire-sym"

version := "0.0.1"

scalaVersion := "2.11.4"

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % "0.9.0",
  "org.spire-math" %% "spire-scalacheck-binding" % "0.9.0",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test"
)

scalacOptions ++= (
  "-deprecation" ::           
  "-encoding" :: "UTF-8" ::
  "-feature" ::                
  "-language:existentials" ::
  "-language:higherKinds" ::
  "-language:implicitConversions" ::
  "-unchecked" ::
  "-Xfatal-warnings" ::       
  "-Xlint" ::
  "-Yno-adapted-args" ::       
  "-Ywarn-dead-code" ::
  "-Ywarn-numeric-widen" ::   
  "-Ywarn-value-discard" ::
  "-Xfuture" ::
  Nil
)
