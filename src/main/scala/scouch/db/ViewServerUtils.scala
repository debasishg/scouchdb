package scouch.db

object ViewServerUtils {
  case class ValidationException(contents: String) extends Exception(contents)
  case class AuthorizationException(contents: String) extends Exception(contents)
  
  import dispatch.json._
  import Js._
  def require(doc: JsValue, field: String) = {
    val s = Symbol(field) ? str
    val s(_s) = doc
    if (doc != null && (_s == "null" || _s.length == 0))
      throw new ValidationException("Field cannot be changed: " + field)
  }
}
