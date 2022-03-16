
name := "NESSE"

version := "1.0"

scalaVersion := "3.1.0"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-encoding","ISO-8859-1",
  "-deprecation"
)

libraryDependencies += "com.formdev" % "flatlaf" % "1.6.4"
libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "3.1.3"
libraryDependencies += "com.jgoodies" % "jgoodies-forms" % "1.9.0"
libraryDependencies += "net.java.jinput" % "jinput" % "2.0.9"
libraryDependencies += "org.yaml" % "snakeyaml" % "1.30"

enablePlugins(JavaAppPackaging)

// Generates Version.scala
Compile / sourceGenerators += Def.task {
  val file = (Compile / scalaSource).value / "ucesoft" / "nes" / "Version.scala"
  println(s"Generating Version.scala $file")
  IO.write(file,
    s"""package ucesoft.nes
       |object Version {
       | val VERSION = "${version.value}"
       | val SCALA_VERSION = "${scalaVersion.value}"
       | val BUILD_DATE = "${java.time.LocalDateTime.now().format(java.time.format.DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss"))}"
       |}
       |""".stripMargin)
  Seq(file)
}.taskValue

maintainer := "abbruzze@gmail.com"

// main class to run
Compile / mainClass := Some("ucesoft.nes.ui.NESUI")
Compile / discoveredMainClasses := Seq()

Universal / javaOptions ++= Seq("-J-Xmx128M", "-J-Xms128M", "-J-server")

batScriptExtraDefines += """set _JAVA_OPTS=%_JAVA_OPTS% -Djava.library.path=%APP_LIB_DIR%"""
bashScriptExtraDefines += """addJava "-Djava.library.path=${app_home}/../lib""""

batScriptExtraDefines += """set _JAVA_OPTS=%_JAVA_OPTS% -Dnesse.home=%APP_HOME%"""
bashScriptExtraDefines += """addJava "-Dnesse.home=${app_home}/../""""

// Universal / packageBin