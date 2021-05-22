val scala2Version = "2.13.4"
val scala3Version = "3.0.0-RC1"

lazy val root = project.in(file(".")).settings(
    name := "psalmd",
    version := "0.1.0",
    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
        ("org.typelevel" %% "cats-core"   %  "2.6.1").withDottyCompat(scalaVersion.value),
        ("org.typelevel" %% "cats-effect" %  "3.1.1").withDottyCompat(scalaVersion.value),
    ),
    
    libraryDependencies ++= Seq(
        ("io.circe" %% "circe-core"  % "0.13.0").withDottyCompat(scalaVersion.value),
        ("io.circe" %% "circe-parser"  % "0.13.0").withDottyCompat(scalaVersion.value),
//        ("io.circe" %% "circe-jawn"  % "0.13.0").withDottyCompat(scalaVersion.value),
//        ("io.circe" %% "circe-derivation"  % "0.13.0-M5").withDottyCompat(scalaVersion.value),
    ),
    
    useScala3doc := true, 
    
    // To cross compile with Dotty and Scala 2
    /*crossScalaVersions := Seq(
        scala3Version,
        scala2Version,
    )*/
)
