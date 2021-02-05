lazy val baseName   = "Lucre-Pi"
lazy val baseNameL  = baseName.toLowerCase
lazy val gitProject = "LucrePi"

lazy val projectVersion = "1.4.0"
lazy val mimaVersion    = "1.4.0"

// ---- dependencies ----

lazy val deps = new {
  val main = new {
    val lucre           = "4.4.1"
    val soundProcesses  = "4.7.0"
    val pi4j            = "1.3"
  }
}

// sonatype plugin requires that these are in global
ThisBuild / version      := projectVersion
ThisBuild / organization := "de.sciss"

lazy val root = project.withId(baseNameL).in(file("."))
  .settings(
    name                 := baseName,
//    version              := projectVersion,
//    organization         := "de.sciss",
    scalaVersion         := "2.13.4",
    crossScalaVersions   := Seq("3.0.0-M3", "2.13.4", "2.12.13"),
    description          := "Raspberry Pi GPIO support for Lucre",
    homepage             := Some(url(s"https://git.iem.at/sciss/$gitProject")),
    licenses             := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
    libraryDependencies ++= Seq(
      "de.sciss"  %% "lucre-expr"           % deps.main.lucre,
      "de.sciss"  %% "soundprocesses-core"  % deps.main.soundProcesses,
      "com.pi4j"  %  "pi4j-core"            % deps.main.pi4j,
      "de.sciss"  %% "lucre-bdb"            % deps.main.lucre     % Test,
    ),
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xlint", "-Xsource:2.13"),
    scalacOptions in (Compile, compile) ++= (if (scala.util.Properties.isJavaAtLeast("9")) Seq("-release", "8") else Nil), // JDK >8 breaks API; skip scala-doc
    // ---- compatibility ----
    mimaPreviousArtifacts := Set("de.sciss" %% baseNameL % mimaVersion),
    updateOptions := updateOptions.value.withLatestSnapshots(false)
  )
  .settings(publishSettings)

// ---- publishing ----
lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  developers := List(
    Developer(
      id    = "sciss",
      name  = "Hanns Holger Rutz",
      email = "contact@sciss.de",
      url   = url("https://www.sciss.de")
    )
  ),
  scmInfo := {
    val h = "git.iem.at"
    val a = s"sciss/$gitProject"
    Some(ScmInfo(url(s"https://$h/$a"), s"scm:git@$h:$a.git"))
  },
)

