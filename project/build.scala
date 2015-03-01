import sbt._
import Keys._

import com.earldouglas.xsbtwebplugin.WebPlugin.webSettings

object JsonLdBuild extends Build {

  val scalaBuildVersion = "2.11.5"

	lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
		version := "0.1.0",
    scalaVersion := scalaBuildVersion,
		scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    testOptions += Tests.Argument(TestFrameworks.JUnit,  "-v"),
    crossScalaVersions := Seq("2.11.5"),
    resolvers += "m2 cental" at "http://central.maven.org/maven2/"
	)
	
  val razDep = "org.obl" %% "raz" % "0.6-SNAPSHOT"

  val servletDep = "javax.servlet" % "servlet-api" % "2.5" % "provided"
    
  val unfilteredVersion = "0.8.2"

  val commonsFileupload = "commons-fileupload" % "commons-fileupload" % "1.2.1"

  val unfilteredDeps = Seq(
    servletDep,
    "net.databinder" %% "unfiltered-filter" % unfilteredVersion,
    "net.databinder" %% "unfiltered-uploads" % unfilteredVersion,
    "net.databinder" %% "unfiltered-filter-uploads" % unfilteredVersion,
    "net.databinder" %% "unfiltered-scalatest" % unfilteredVersion % "test") ++ Seq(commonsFileupload)

  lazy val shapelessDep = "com.chuusai" %% "shapeless" % "2.1.0" 

  lazy val javaJsonLd = "com.github.jsonld-java" % "jsonld-java" % "0.5.1"
    
  lazy val jsonLdProject = Project(
		id = "json-ld",
		base = file("json-ld"),
		settings = sharedSettings ++ Seq(
          organization := "org.obl",
          libraryDependencies += shapelessDep,
          libraryDependencies += javaJsonLd,
          libraryDependencies += razDep
        )
	) 
    
  lazy val root= Project(
		id = "json-ld-root",
		base = file("."),
    settings = sharedSettings ++ Seq(
      organization := "org.obl"
    )
  ) aggregate(jsonLdProject)

}