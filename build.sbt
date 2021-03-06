val connIdVersion = "1.4.2.35"

lazy val root = (project in file("."))
  .enablePlugins(GitVersioning, AutomateHeaderPlugin)
  .settings(
    organization := "eu.arthepsy.midpoint",
    name := "connid-common",
    description := "Common utils library for ConnId connectors",
    scalaVersion := "2.12.6",
    crossScalaVersions := Seq(scalaVersion.value,
                              "2.11.12",
                              "2.13.0-M3",
                              "0.8.0-RC1"),
    resolvers ++= Seq(
      "Evolveum releases" at "https://nexus.evolveum.com/nexus/content/repositories/releases/",
      "Evolveum snapshots" at "https://nexus.evolveum.com/nexus/content/repositories/snapshots/"
    ),
    libraryDependencies ++= Seq(
      "net.tirasa.connid" % "connector-framework" % connIdVersion % Provided.name,
      "net.tirasa.connid" % "connector-framework-contract" % connIdVersion % Provided.name
    ),
    fork in Test := true,
    libraryDependencies ++= (scalaVersion.value match {
      case "0.8.0-RC1" =>
        Seq("org.scalatest" % "scalatest_2.12" % "3.0.5" % "test")
      case "2.13.0-M3" =>
        Seq("org.scalatest" % "scalatest_2.12" % "3.0.5" % "test")
      case _ => Seq("org.scalatest" %% "scalatest" % "3.0.5" % "test")
    }),
    libraryDependencies ++= Seq(
      "org.mockito" % "mockito-core" % "2.18.3" % "test",
      "com.github.tomakehurst" % "wiremock" % "2.17.0" % "test"
    ),
    git.baseVersion := "1.0",
    git.useGitDescribe := true,
    git.uncommittedSignifier := None,
    bintrayOrganization := None,
    bintrayRepository := "maven",
    publishMavenStyle := true,
    publishArtifact in Test := true,
    PgpKeys.useGpg in Global := true,
    PgpKeys.gpgCommand in Global := "gpg2",
    licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT")),
    headerLicense := Some(
      HeaderLicense.Custom(IO.read(baseDirectory.value / "LICENSE.md")))
  )
