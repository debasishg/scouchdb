import sbt._

class SCouchDbProject(info: ProjectInfo) extends DefaultProject(info) 
{
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val scalaToolsReleases = "Scala-Tools Maven2 Releases Repository" at "http://scala-tools.org/repo-releases"
  val dispatch_json = "net.databinder" % "dispatch-json_2.9.0-1" % "0.8.3" % "compile"
  val dispatch_http_json = "net.databinder" % "dispatch-http-json_2.9.0-1" % "0.8.3" % "compile"
  val dispatch_http = "net.databinder" % "dispatch-http_2.9.0-1" % "0.8.3" % "compile"
  val commons_logging = "commons-logging" % "commons-logging" % "1.1.1" % "compile"
  val httpclient = "org.apache.httpcomponents" % "httpclient" % "4.1.1" % "compile"
  val sjson = "net.debasishg" % "sjson_2.9.0-1" % "0.13" % "compile"

  val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"
  val junit = "junit" % "junit" % "4.8.1"
}
