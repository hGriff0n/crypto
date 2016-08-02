lazy val root = (project in file(".")).
settings(
    name := "crypto",
    version := "0.5"
)

libraryDependencies += "org.scalanlp" %% "breeze" % "0.12"
libraryDependencies += "org.scalanlp" %% "breeze-natives" % "0.12"
libraryDependencies += "org.spire-math" %% "spire" % "0.11.0"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
