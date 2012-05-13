name := "scouchdb"

organization := "scouch.db"

version := "0.7"

resolvers ++= Seq("Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots",
                  "Scala-Tools Maven2 Releases Repository" at "http://scala-tools.org/repo-releases")

libraryDependencies ++= Seq("net.databinder" %% "dispatch-json" % "0.8.8" % "compile",
                            "net.databinder" %% "dispatch-http-json" % "0.8.8" % "compile",
                            "net.databinder" %% "dispatch-http" % "0.8.8" % "compile",
                            "commons-logging" % "commons-logging" % "1.1.1" % "compile",
                            "org.apache.httpcomponents" % "httpclient" % "4.1.3" % "compile",
                            "net.debasishg" %% "sjson" % "0.17" % "compile",
                            "org.scala-lang" % "scala-compiler" % "2.9.1" % "compile",
                            "org.scalatest" %% "scalatest" % "1.6.1" % "test",
                            "junit" % "junit" % "4.10" % "test")
