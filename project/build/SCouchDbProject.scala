import sbt._

class SCouchDbProject(info: ProjectInfo) extends DefaultProject(info) 
{
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val scalaToolsReleases = "Scala-Tools Maven2 Releases Repository" at "http://scala-tools.org/repo-releases"
  // val embeddedRepo = MavenRepository("Embedded Repo", (info.projectPath / "embedded-repo").asURL.toString)
  val dispatch_json = "net.databinder" % "dispatch-json_2.9.0" % "0.8.1" % "compile"
  val dispatch_http_json = "net.databinder" % "dispatch-http-json_2.9.0" % "0.8.1" % "compile"
  val dispatch_http = "net.databinder" % "dispatch-http_2.9.0" % "0.8.1" % "compile"
  val commons_logging = "commons-logging" % "commons-logging" % "1.1.1" % "compile"
  val httpclient = "org.apache.httpcomponents" % "httpclient" % "4.0.1" % "compile"
  val sjson = "net.debasishg" % "sjson_2.9.0" % "0.11" % "compile"

  val scalatest =
    buildScalaVersion match {
      case "2.8.0" =>
        "org.scalatest" % "scalatest" % "1.2" % "test"
      case "2.9.0" =>
        "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "test"
    }

  val junit = "junit" % "junit" % "4.8.1"
}
