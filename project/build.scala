import sbt._
import Keys._

object BildajBuild extends Build {
  lazy val core = Project("core", file("core"), settings = Defaults.defaultSettings ++ 
    Seq(libraryDependencies ++= 
      Seq(
        "org.apache.spark" %% "spark-core" % "1.0.2",
        "com.fasterxml.jackson.core" % "jackson-core" % "2.4.1.1",
        "org.scalatest" %% "scalatest" % "2.2.1" % "test",
        "org.apache.jena" % "jena-arq" % "2.12.0",
        "com.github.spullara.mustache.java" % "compiler" % "0.8.16"
      )
    )
  )

  lazy val tools_fastalign = Project(id = "tools-fastalign", base = file("tools-fastalign"))

}
