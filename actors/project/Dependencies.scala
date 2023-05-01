import sbt._

object Dependencies {

  object V {
    val AkkaVersion = "2.7.0"
    val LogBackVersion = "1.3.7"
  }
  
  object Libraries{
    import V._
    val logback = "ch.qos.logback" % "logback-classic" % LogBackVersion 
    val akka = "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion
    val akkaTestkit = "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test
  }
}