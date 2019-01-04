name := "disneyworld-fpp"

version := "0.1"

scalaVersion := "2.12.8"

val seleniumVersion = "3.141.59"

lazy val root = project.in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.seleniumhq.selenium" % "selenium-chrome-driver" % seleniumVersion,
      "org.seleniumhq.selenium" % "selenium-support" % seleniumVersion,
      "org.datacrafts" %% "noschema-json" % "0.2.6",
      "com.typesafe" % "config" % "1.3.3",
      "org.slf4j" % "slf4j-log4j12" % "1.7.25"
    )
  )
