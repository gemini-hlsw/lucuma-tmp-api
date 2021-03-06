val attoVersion                 = "0.9.5"
val catsEffectVersion           = "3.1.1"
val catsMtlVersion              = "1.2.1"
val catsTestkitScalaTestVersion = "2.1.5"
val catsVersion                 = "2.6.1"
val catsScalacheckVersion       = "0.3.0"
val catsTimeVersion             = "0.3.4"
val circeOpticsVersion          = "0.14.1"
val circeVersion                = "0.14.1"
val cirisVersion                = "2.0.1"
val clueVersion                 = "0.16.0"
val http4sVersion               = "0.23.0-RC1"
val fs2Version                  = "3.0.6"
val jawnVersion                 = "1.2.0"
val kindProjectorVersion        = "0.13.0"
val logbackVersion              = "1.2.4"
val lucumaCoreVersion           = "0.11.0"
// val lucumaSsoVersion            = "0.0.9" AWAITING CE3
val log4catsVersion             = "2.1.1"
val monocleVersion              = "3.0.0"
val refinedVersion              = "0.9.27"
val sangriaVersion              = "2.1.3"
val sangriaCirceVersion         = "1.3.2"
val singletonOpsVersion         = "0.5.2"

val munitVersion                = "0.7.27"
val disciplineMunitVersion      = "1.0.9"


inThisBuild(
  Seq(
    homepage := Some(url("https://github.com/gemini-hlsw")),
    addCompilerPlugin(
      ("org.typelevel" % "kind-projector" % kindProjectorVersion).cross(CrossVersion.full)
    )
  ) ++ lucumaPublishSettings
)

lazy val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel"     %% "cats-testkit"           % catsVersion                 % "test",
    "org.typelevel"     %% "cats-testkit-scalatest" % catsTestkitScalaTestVersion % "test"
  ),
  scalacOptions --= Seq("-Xfatal-warnings").filterNot(_ => insideCI.value)
)

lazy val noPublishSettings = Seq(
  publish / skip := true
)

lazy val modules: List[ProjectReference] = List(
  core,
  service
)

lazy val `gem-odb-api` = project.in(file("."))
  .settings(commonSettings)
  .settings(noPublishSettings)
  .aggregate(modules:_*)
  .disablePlugins(RevolverPlugin)

lazy val core = project
  .in(file("modules/core"))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(commonSettings)
  .settings(
    name := "lucuma-odb-api-core",
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    ),
    libraryDependencies ++= Seq(
      "co.fs2"                     %% "fs2-core"                  % fs2Version,
      "dev.optics"                 %% "monocle-core"              % monocleVersion,
      "dev.optics"                 %% "monocle-state"             % monocleVersion,
      "dev.optics"                 %% "monocle-macro"             % monocleVersion,
      "org.sangria-graphql"        %% "sangria"                   % sangriaVersion,
      "org.sangria-graphql"        %% "sangria-circe"             % sangriaCirceVersion,
      "edu.gemini"                 %% "clue-model"                % clueVersion,
      "edu.gemini"                 %% "lucuma-core"               % lucumaCoreVersion,
//      "edu.gemini"                 %% "lucuma-sso-backend-client" % lucumaSsoVersion,
      "org.tpolecat"               %% "atto-core"                 % attoVersion,
      "org.typelevel"              %% "cats-core"                 % catsVersion,
      "org.typelevel"              %% "cats-effect"               % catsEffectVersion,
      "org.typelevel"              %% "cats-mtl"                  % catsMtlVersion,
      "io.chrisdavenport"          %% "cats-time"                 % catsTimeVersion,
      "io.circe"                   %% "circe-core"                % circeVersion,
      "io.circe"                   %% "circe-literal"             % circeVersion,
      "io.circe"                   %% "circe-optics"              % circeOpticsVersion,
      "io.circe"                   %% "circe-parser"              % circeVersion,
      "io.circe"                   %% "circe-generic"             % circeVersion,
      "io.circe"                   %% "circe-generic-extras"      % circeVersion,
      "io.circe"                   %% "circe-refined"             % circeVersion,
      "org.typelevel"              %% "jawn-parser"               % jawnVersion,
      "org.typelevel"              %% "log4cats-slf4j"            % log4catsVersion,
      "ch.qos.logback"             %  "logback-classic"           % logbackVersion,
      "eu.timepit"                 %% "singleton-ops"             % singletonOpsVersion,
      "eu.timepit"                 %% "refined"                   % refinedVersion,
      "eu.timepit"                 %% "refined-cats"              % refinedVersion,


      "edu.gemini"                 %% "lucuma-core-testkit"       % lucumaCoreVersion      % Test,
      "io.chrisdavenport"          %% "cats-scalacheck"           % catsScalacheckVersion  % Test,
      "org.scalameta"              %% "munit"                     % munitVersion           % Test,
      "org.typelevel"              %% "discipline-munit"          % disciplineMunitVersion % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )

lazy val service = project
  .in(file("modules/service"))
  .enablePlugins(AutomateHeaderPlugin)
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "lucuma-odb-api-service",
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    ),
    libraryDependencies ++= Seq(
      "dev.optics"                 %% "monocle-core"              % monocleVersion,
      "org.sangria-graphql"        %% "sangria"                   % sangriaVersion,
      "org.sangria-graphql"        %% "sangria-circe"             % sangriaCirceVersion,
      "edu.gemini"                 %% "clue-model"                % clueVersion,
      "edu.gemini"                 %% "lucuma-core"               % lucumaCoreVersion,
//      "edu.gemini"                 %% "lucuma-sso-backend-client" % lucumaSsoVersion,
      "org.tpolecat"               %% "atto-core"                 % attoVersion,
      "org.typelevel"              %% "cats-core"                 % catsVersion,
      "org.typelevel"              %% "cats-effect"               % catsEffectVersion,
      "io.circe"                   %% "circe-core"                % circeVersion,
      "io.circe"                   %% "circe-literal"             % circeVersion,
      "io.circe"                   %% "circe-optics"              % circeOpticsVersion,
      "io.circe"                   %% "circe-parser"              % circeVersion,
      "io.circe"                   %% "circe-generic"             % circeVersion,
      "io.circe"                   %% "circe-generic-extras"      % circeVersion,
      "is.cir"                     %% "ciris"                     % cirisVersion,
      "org.typelevel"              %% "jawn-parser"               % jawnVersion,
      "org.typelevel"              %% "log4cats-slf4j"            % log4catsVersion,
      "ch.qos.logback"             %  "logback-classic"           % logbackVersion,
      "org.http4s"                 %% "http4s-core"               % http4sVersion,
      "org.http4s"                 %% "http4s-blaze-server"       % http4sVersion,
      "org.http4s"                 %% "http4s-blaze-client"       % http4sVersion,
      "org.http4s"                 %% "http4s-circe"              % http4sVersion,
      "org.http4s"                 %% "http4s-dsl"                % http4sVersion
    )
  ).enablePlugins(JavaAppPackaging)
