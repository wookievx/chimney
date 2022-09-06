import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val versions = new {
  val scala212 = "2.12.12"
  val scala213 = "2.13.5"
}

val scala3Version = "3.1.3"

val settings = Seq(
  version := "0.6.1",
  scalaVersion := versions.scala213,
  crossScalaVersions := Seq(versions.scala212, versions.scala213),
  scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-encoding", "UTF-8",
    "-unchecked",
    "-deprecation",
    "-explaintypes",
    "-feature",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Xlint:adapted-args",
    "-Xlint:delayedinit-select",
    "-Xlint:doc-detached",
    "-Xlint:inaccessible",
    "-Xlint:infer-any",
    "-Xlint:nullary-unit",
    "-Xlint:option-implicit",
    "-Xlint:package-object-classes",
    "-Xlint:poly-implicit-overload",
    "-Xlint:private-shadow",
    "-Xlint:stars-align",
    "-Xlint:type-parameter-shadow",
    "-Ywarn-unused:locals",
    "-Ywarn-macros:after",
    "-Xfatal-warnings",
    "-language:higherKinds"
  ),
  scalacOptions ++= (
    if (scalaVersion.value >= "2.13")
      Seq("-Wunused:patvars")
    else
      Seq(
        "-Xfuture",
        "-Xexperimental",
        "-Yno-adapted-args",
        "-Ywarn-inaccessible",
        "-Ywarn-infer-any",
        "-Ywarn-nullary-override",
        "-Ywarn-nullary-unit",
        "-Xlint:by-name-right-associative",
        "-Xlint:unsound-match",
        "-Xlint:nullary-override"
      )
    ),
  scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")
)

val scala3Settings = Seq(
  testFrameworks += new TestFramework("utest.runner.Framework"),
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.10" % "test",
  version := "0.1.0",
  scalaVersion := scala3Version,
  scalacOptions ++= Seq(
    // "-Xtarget:1.8",
    "-encoding", "UTF-8",
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-Xmax-inlines", "64"
  )
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %%% "scala-collection-compat" % "2.4.2",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    "com.lihaoyi" %%% "utest" % "0.7.7" % "test"
  )
)

lazy val root = project
  .in(file("."))
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(noPublishSettings: _*)
  .aggregate(chimneyJVM, chimneyJS, chimneyCatsJVM, chimneyCatsJS, chimney3)
  .dependsOn(chimneyJVM, chimneyJS, chimneyCatsJVM, chimneyCatsJS, chimney3)
  .enablePlugins(SphinxPlugin, GhpagesPlugin)
  .settings(
    Sphinx / version := version.value,
    Sphinx / sourceDirectory := file("docs") / "source",
    git.remoteRepo := "git@github.com:scalalandio/chimney.git"
  )

lazy val chimney = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(protos % "test->test")
  .settings(
    moduleName := "chimney",
    name := "chimney",
    description := "Scala library for boilerplate free data rewriting",
    testFrameworks += new TestFramework("utest.runner.Framework"),
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.full)
  )
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(dependencies: _*)

lazy val chimneyJVM = chimney.jvm
lazy val chimneyJS = chimney.js

lazy val chimneyCats = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(chimney % "test->test;compile->compile")
  .settings(
    moduleName := "chimney-cats",
    name := "chimney-cats",
    description := "Chimney module for validated transformers support",
    testFrameworks += new TestFramework("utest.runner.Framework"),
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.full)
  )
  .settings(settings: _*)
  .settings(publishSettings: _*)
  .settings(dependencies: _*)
  .settings(libraryDependencies += "org.typelevel" %%% "cats-core" % "2.4.2" % "provided")

lazy val chimneyCatsJVM = chimneyCats.jvm
lazy val chimneyCatsJS = chimneyCats.js

lazy val protos = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(
    moduleName := "chimney-protos",
    name := "chimney-protos"
  )
  .settings(settings: _*)
  .settings(noPublishSettings: _*)

lazy val protosJVM = protos.jvm
lazy val protosJS = protos.js

lazy val chimney3 = project.in(file("chimney3"))
  // .crossType(CrossType.Pure)
  .settings(
    moduleName := "chimney3",
    name := "chimney3",
    description := "Scala 3 library for boilerplate free data rewriting "
  )
  .settings(scala3Settings: _*)
  .settings(publishSettings: _*)


lazy val publishSettings = Seq(
  organization := "io.scalaland",
  homepage := Some(url("https://scalaland.io")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(
    ScmInfo(url("https://github.com/scalalandio/chimney"), "scm:git:git@github.com:scalalandio/chimney.git")
  ),
  publishTo := sonatypePublishToBundle.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ =>
    false
  },
  pomExtra := (
    <developers>
      <developer>
        <id>krzemin</id>
        <name>Piotr Krzemiński</name>
        <url>http://github.com/krzemin</url>
      </developer>
      <developer>
        <id>MateuszKubuszok</id>
        <name>Mateusz Kubuszok</name>
        <url>http://github.com/MateuszKubuszok</url>
      </developer>
      <developer>
        <id>wookievx</id>
        <name>Łukasz Lampart</name>
        <url>https://github.com/wookievx</url>
      </developer>
    </developers>
  )
)

lazy val noPublishSettings =
  Seq(skip in publish := true, publishArtifact := false)
