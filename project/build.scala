import sbt._
import Keys._

import com.earldouglas.xsbtwebplugin.WebPlugin.webSettings

object JsonLdBuild extends Build {

  val scalaBuildVersion = "2.11.6"

	lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
		version := "0.1.0",
    scalaVersion := scalaBuildVersion,
		scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    testOptions += Tests.Argument(TestFrameworks.JUnit,  "-v"),
    crossScalaVersions := Seq(scalaBuildVersion),
    resolvers += "m2 cental" at "http://central.maven.org/maven2/",
    
    organization := "org.obl"
	)

  val razVersion = "0.7-SNAPSHOT"  

  val razDep = "org.obl" %% "raz" % razVersion
  val razUnfilteredDep = "org.obl" %% "raz-unfiltered" % razVersion

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
          libraryDependencies += shapelessDep,
          libraryDependencies += javaJsonLd,
          libraryDependencies += razDep
        )
	) 
  
   lazy val hydraProject = Project(
		id = "json-ld-hydra",
		base = file("json-ld-hydra"),
		settings = sharedSettings ++ Seq(
          //libraryDependencies += javaJsonLd,
          //libraryDependencies += razDep,
          //libraryDependencies += shapelessDep
        )
	)  dependsOn jsonLdProject
 
  
   lazy val jsonLdWebProject = Project(
      id = "json-ld-web",
      base = file("json-ld-web"),
      settings = sharedSettings ++ webSettings ++ Seq(
          organization := "org.obl",
          libraryDependencies += razDep,
          libraryDependencies += razUnfilteredDep,
          libraryDependencies ++= unfilteredDeps,
          libraryDependencies ++= Seq(
            "org.eclipse.jetty" % "jetty-webapp" % "9.1.0.v20131115" % "container",
            "org.eclipse.jetty" % "jetty-plus"   % "9.1.0.v20131115" % "container"
          )
        )
	) dependsOn (jsonLdProject, hydraProject)

    
  lazy val root= Project(
		id = "json-ld-root",
		base = file("."),
    settings = sharedSettings ++ Seq(
      organization := "org.obl"
    )
  ) aggregate(jsonLdProject, hydraProject, jsonLdWebProject)

}