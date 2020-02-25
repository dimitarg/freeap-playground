name := "freeap-playground"

version := "0.1"

scalaVersion := "2.13.1"

lazy val root  = (project in file("."))
  .settings(
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "2.1.1",
      "org.typelevel" %% "cats-free" % "2.1.1"

    )
  )


