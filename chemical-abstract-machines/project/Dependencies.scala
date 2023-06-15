import sbt._

object Dependencies {

  object V {
    val LogBackVersion = "1.3.7"
  }
  
  object Libraries{
    import V._
    val logback = "ch.qos.logback" % "logback-classic" % LogBackVersion 
  }
}