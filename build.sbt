lazy val coverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false
)

lazy val buildSettings = Seq(
  organization := "com.ithaca",
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.11.8",
  name         := "hephaestus",
  version      := "0.1.0-SNAPSHOT"
)

lazy val commonScalacOptions = Seq(
  "-encoding", "UTF-8",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-language:postfixOps",
  "-Ypartial-unification",
  "-Yliteral-types"
)

lazy val commonResolvers = Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.jcenterRepo
)

lazy val commonSettings = Seq(
    resolvers := commonResolvers,
    scalacOptions ++= commonScalacOptions
) ++ coverageSettings ++ buildSettings


lazy val core = (project in file("core"))
  .settings(
    moduleName := "hephaestus-core",
    commonSettings,
    target in javah := (target in LocalProject("native")).value / "include"
).dependsOn(native % Runtime)

lazy val native = (project in file("native"))
  .enablePlugins(JniNative)
  .settings(
    moduleName := "hephaestus-native",
    buildSettings,
    sourceDirectory in nativeCompile := baseDirectory.value,
    compile in Compile := {
      Def.sequential(
        javah in LocalProject("core"),
        nativeCompile
      ).value
      (compile in Compile).value
    }
)

lazy val root = (project in file(".")).aggregate(core, native)
