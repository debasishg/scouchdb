import sbt._

class SCouchDbProject(info: ProjectInfo) extends DefaultProject(info) 
{
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val scalaToolsReleases = "Scala-Tools Maven2 Releases Repository" at "http://scala-tools.org/repo-releases"
  // val embeddedRepo = MavenRepository("Embedded Repo", (info.projectPath / "embedded-repo").asURL.toString)
  val dispatch_json = "net.databinder" % "dispatch-json_2.8.0" % "0.7.4" % "compile"
  val dispatch_http_json = "net.databinder" % "dispatch-http-json_2.8.0" % "0.7.4" % "compile"
  val dispatch_http = "net.databinder" % "dispatch-http_2.8.0" % "0.7.4" % "compile"
  val commons_logging = "commons-logging" % "commons-logging" % "1.1.1" % "compile"
  val httpclient = "org.apache.httpcomponents" % "httpclient" % "4.0.1" % "compile"
  val sjson = "sjson.json" % "sjson" % "0.7" % "compile"

  val scalatest =
    buildScalaVersion match {
      case "2.7.7" => 
        "org.scalatest" % "scalatest" % "1.0" 
      case "2.8.0.Beta1" =>
        "org.scalatest" % "scalatest" % "1.0.1-for-scala-2.8.0.Beta1-with-test-interfaces-0.3-SNAPSHOT" 
      case "2.8.0.RC2" =>
        "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC2-SNAPSHOT" % "test"
      case "2.8.0.RC3" =>
        "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC2-SNAPSHOT" % "test"
      case "2.8.0.RC7" =>
        "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC7-SNAPSHOT" % "test"
      case "2.8.0" =>
        "org.scalatest" % "scalatest" % "1.2" % "test"
    }

  val junit = "junit" % "junit" % "4.8.1"
}
