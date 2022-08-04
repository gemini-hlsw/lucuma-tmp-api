val attoVersion                 = "0.9.5"
val catsEffectVersion           = "3.3.14"
val catsTestkitScalaTestVersion = "2.1.5"
val catsVersion                 = "2.8.0"
val catsScalacheckVersion       = "0.3.1"
val catsTimeVersion             = "0.5.0"
val circeOpticsVersion          = "0.14.1"
val circeExtrasVersion          = "0.14.2"
val circeVersion                = "0.14.2"
val cirisVersion                = "2.3.3"
val clueVersion                 = "0.23.1"
val http4sVersion               = "0.23.13"
val http4sBlazeVersion          = "0.23.12"
val http4sJdkHttpClientVersion  = "0.7.0"
val fs2Version                  = "3.2.10"
val jawnVersion                 = "1.4.0"
val kindProjectorVersion        = "0.13.2"
val logbackVersion              = "1.2.11"
val lucumaCoreVersion           = "0.43.1"
val lucumaGraphQLRoutesVersion  = "0.3.4"
//val lucumaSsoVersion            = "0.0.11"
val log4catsVersion             = "2.4.0"
val monocleVersion              = "3.1.0"
val munitCatsEffectVersion      = "1.0.7"
val refinedVersion              = "0.10.1"
val sangriaVersion              = "3.2.0"
val sangriaCirceVersion         = "1.3.2"
val singletonOpsVersion         = "0.5.2"

val munitVersion                = "0.7.29"
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
      "-Ymacro-annotations",
      "-Ywarn-macros:after"
    ),
    libraryDependencies ++= Seq(
      "co.fs2"                     %% "fs2-core"                  % fs2Version,
      "dev.optics"                 %% "monocle-core"              % monocleVersion,
      "dev.optics"                 %% "monocle-state"             % monocleVersion,
      "dev.optics"                 %% "monocle-macro"             % monocleVersion,
      "org.sangria-graphql"        %% "sangria"                   % sangriaVersion,
      "org.sangria-graphql"        %% "sangria-circe"             % sangriaCirceVersion,
      "edu.gemini"                 %% "clue-core"                 % clueVersion,
      "edu.gemini"                 %% "clue-http4s"               % clueVersion,
      "edu.gemini"                 %% "clue-model"                % clueVersion,
      "org.http4s"                 %% "http4s-jdk-http-client"    % http4sJdkHttpClientVersion,
      "edu.gemini"                 %% "lucuma-core"               % lucumaCoreVersion,
      "org.tpolecat"               %% "atto-core"                 % attoVersion,
      "org.typelevel"              %% "cats-core"                 % catsVersion,
      "org.typelevel"              %% "cats-effect"               % catsEffectVersion,
      "org.typelevel"              %% "cats-time"                 % catsTimeVersion,
      "io.circe"                   %% "circe-core"                % circeVersion,
      "io.circe"                   %% "circe-literal"             % circeVersion,
      "io.circe"                   %% "circe-optics"              % circeOpticsVersion,
      "io.circe"                   %% "circe-parser"              % circeVersion,
      "io.circe"                   %% "circe-generic"             % circeVersion,
      "io.circe"                   %% "circe-generic-extras"      % circeExtrasVersion,
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
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name := "lucuma-odb-api-service",
    scalacOptions ++= Seq(
      "-Ymacro-annotations"
    ),
    libraryDependencies ++= Seq(
      "dev.optics"                 %% "monocle-core"                  % monocleVersion,
      "org.sangria-graphql"        %% "sangria"                       % sangriaVersion,
      "org.sangria-graphql"        %% "sangria-circe"                 % sangriaCirceVersion,
      "edu.gemini"                 %% "clue-model"                    % clueVersion,
      "edu.gemini"                 %% "lucuma-core"                   % lucumaCoreVersion,
      "org.tpolecat"               %% "atto-core"                     % attoVersion,
      "org.typelevel"              %% "cats-core"                     % catsVersion,
      "org.typelevel"              %% "cats-effect"                   % catsEffectVersion,
      "io.circe"                   %% "circe-core"                    % circeVersion,
      "io.circe"                   %% "circe-literal"                 % circeVersion,
      "io.circe"                   %% "circe-optics"                  % circeOpticsVersion,
      "io.circe"                   %% "circe-parser"                  % circeVersion,
      "io.circe"                   %% "circe-generic"                 % circeVersion,
      "io.circe"                   %% "circe-generic-extras"          % circeExtrasVersion,
      "is.cir"                     %% "ciris"                         % cirisVersion,
      "org.typelevel"              %% "jawn-parser"                   % jawnVersion,
      "org.typelevel"              %% "log4cats-slf4j"                % log4catsVersion,
      "ch.qos.logback"             %  "logback-classic"               % logbackVersion,
      "org.http4s"                 %% "http4s-core"                   % http4sVersion,
      "org.http4s"                 %% "http4s-blaze-server"           % http4sBlazeVersion,
      "org.http4s"                 %% "http4s-blaze-client"           % http4sBlazeVersion,
      "org.http4s"                 %% "http4s-circe"                  % http4sVersion,
      "org.http4s"                 %% "http4s-dsl"                    % http4sVersion,
      "edu.gemini"                 %% "clue-http4s"                   % clueVersion            % Test,
      "org.typelevel"              %% "munit-cats-effect-3"           % munitCatsEffectVersion % Test,
//      "edu.gemini"                 %% "lucuma-sso-backend-client"     % lucumaSsoVersion,
      "edu.gemini"                 %% "lucuma-graphql-routes-sangria" % lucumaGraphQLRoutesVersion,
    ),
    testFrameworks += new TestFramework("munit.Framework"),
    Test / parallelExecution := false, // tests run fine in parallel but output is nicer this way
  ).enablePlugins(JavaAppPackaging)
