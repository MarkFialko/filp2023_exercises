name := "exercises"

version := "0.1"

inThisBuild {
  List(
    scalaVersion := "2.13.10",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixScalaBinaryVersion := (ThisBuild / scalaBinaryVersion).value,
    scalafixDependencies ++= Seq(
      "com.github.vovapolu" %% "scaluzzi" % "0.1.23"
    )
  )
}

val libraries = Seq(
  "org.scalatest"     %% "scalatest"       % "3.2.3"         % Test,
  "org.scalatestplus" %% "scalacheck-1-15" % "3.3.0.0-SNAP3" % Test
)

val kindProjectorDep = "org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full

// Добавляем пакеты по мере прохождения курса
lazy val exercises01 = project in file("exercises01") settings (libraryDependencies ++= libraries)
lazy val exercises02 = project in file("exercises02") settings (libraryDependencies ++= libraries)
lazy val exercises03 = project in file("exercises03") settings (libraryDependencies ++= libraries)
lazy val exercises04 = project in file("exercises04") settings (libraryDependencies ++= libraries)
lazy val exercises05 = project in file("exercises05") settings (libraryDependencies ++= libraries)
lazy val exercises06 = project in file("exercises06") settings {
  libraryDependencies ++= libraries
} settings {
  scalacOptions ++= Seq("-feature", "-language:implicitConversions")
}
lazy val exercises07 = project in file("exercises07") settings {
  libraryDependencies ++= libraries
} settings {
  scalacOptions ++= Seq("-feature", "-language:implicitConversions")
  addCompilerPlugin(kindProjectorDep)
}
lazy val exercises08 = project in file("exercises08") settings {
  libraryDependencies ++= libraries
} settings {
  scalacOptions ++= Seq("-feature", "-language:implicitConversions")
  addCompilerPlugin(kindProjectorDep)
}
lazy val exercises09 = project in file("exercises09") settings (
  libraryDependencies ++=
    "org.typelevel"   %% "cats-effect"                   % "3.4.0" +:
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test +:
      libraries
)
lazy val exercises10 = project in file("exercises10") settings {
  libraryDependencies ++= libraries :+ "org.typelevel" %% "cats-effect" % "2.4.1"
} settings {
  scalacOptions ++= Seq("-feature", "-language:implicitConversions", "-Xfatal-warnings")
  addCompilerPlugin(kindProjectorDep)
}
lazy val exercises11 = project in file("exercises11") settings {
  libraryDependencies ++= libraries ++ Seq(
    "com.beachape"                  %% "enumeratum"                         % "1.6.1",
    "com.beachape"                  %% "enumeratum-circe"                   % "1.6.1",
    "org.tpolecat"                  %% "doobie-core"                        % "0.13.3",
    "org.tpolecat"                  %% "doobie-postgres"                    % "0.13.3",
    "org.tpolecat"                  %% "doobie-hikari"                      % "0.13.3",
    "org.tpolecat"                  %% "doobie-specs2"                      % "0.12.1" % Test,
    "com.h2database"                % "h2"                                  % "1.4.200" % Test,
    "com.iheart"                    %% "ficus"                              % "1.4.7",
    "io.circe"                      %% "circe-parser"                       % "0.13.0",
    "io.circe"                      %% "circe-generic"                      % "0.13.0",
    "com.softwaremill.sttp.client3" %% "core"                               % "3.3.4",
    "com.softwaremill.sttp.client3" %% "async-http-client-backend-cats-ce2" % "3.3.4",
    "io.estatico"                   %% "newtype"                            % "0.4.4",
    "com.github.valskalla"          %% "odin-core"                          % "0.11.0",
    "org.slf4j"                     % "slf4j-api"                           % "1.7.30",
    "org.slf4j"                     % "slf4j-simple"                        % "1.7.30",
    //  for http server
    "com.softwaremill.sttp.tapir" %% "tapir-http4s-server"      % "0.17.19",
    "com.softwaremill.sttp.tapir" %% "tapir-json-circe"         % "0.17.19",
    "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-http4s"  % "0.17.19",
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs"       % "0.17.19",
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe-yaml" % "0.17.19"
  )
} settings {
  addCompilerPlugin(kindProjectorDep)
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
} settings {
  scalacOptions ++= Seq("-feature", "-language:implicitConversions", "-Xfatal-warnings", "-Ymacro-annotations")
}

lazy val lection02 = project in file("lection02") settings (libraryDependencies ++= libraries)
lazy val lection03 = project in file("lection03") settings (libraryDependencies ++= libraries)
lazy val lection04 = project in file("lection04") settings (libraryDependencies ++= libraries)
lazy val lection05 = project in file("lection05") settings (libraryDependencies ++= libraries)
lazy val lection06 = project in file("lection06") settings (libraryDependencies ++= libraries)
lazy val lection07 = project in file("lection07") settings (libraryDependencies ++= libraries) settings {
  scalacOptions ++= Seq("-feature", "-language:implicitConversions")
  addCompilerPlugin(kindProjectorDep)
}
lazy val lection08 = project in file("lection08") settings (libraryDependencies ++= libraries) settings {
  scalacOptions ++= Seq("-feature", "-language:implicitConversions")
  addCompilerPlugin(kindProjectorDep)
}

lazy val lection09 = project in file("lection09") settings (
  libraryDependencies ++=
    "org.typelevel"   %% "cats-effect"                   % "3.4.0" +:
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test +:
      libraries
)
lazy val lection10 = project in file("lection10") settings {
  libraryDependencies ++= libraries :+ ("org.typelevel" %% "cats-effect" % "2.4.1")
} settings {
  scalacOptions ++= Seq("-feature", "-language:implicitConversions", "-Xfatal-warnings", "-language:postfixOps")
  addCompilerPlugin(kindProjectorDep)
}
