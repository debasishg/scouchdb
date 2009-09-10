package scouch.db

object Driver {
  def main(args: Array[String]) {
    (new SCouchDbSpec).execute()
    (new ScalaViewServerSpec).execute()
    (new ViewServerWithObjectsSpec).execute()
    (new ScalaValidationSpec).execute()
  }
}
