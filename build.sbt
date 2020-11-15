lazy val baseName   = "Lucre-Pi"
lazy val baseNameL  = baseName.toLowerCase
lazy val gitProject = "LucrePi"

lazy val projectVersion = "1.1.0"
lazy val mimaVersion    = "1.1.0"

// ---- dependencies ----

lazy val deps = new {
  val main = new {
    val lucre           = "4.2.0"
    val soundProcesses  = "4.3.0"
    val pi4j            = "1.2"
  }
}

lazy val root = project.withId(baseNameL).in(file("."))
  .settings(
    name                 := baseName,
    version              := projectVersion,
    organization         := "de.sciss",
    scalaVersion         := "2.13.3",
    crossScalaVersions   := Seq("2.13.3", "2.12.12"),
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
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := { val n = gitProject
<scm>
  <url>git@git.iem.at:sciss/{n}.git</url>
  <connection>scm:git:git@git.iem.at:sciss/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
  }
)
