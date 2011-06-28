package scouch.db

import sjson.json._ 
import scala.reflect._
import scala.annotation.target._

sealed abstract class IdempotentFns(name: String)
case object Lists extends IdempotentFns("lists")
case object Shows extends IdempotentFns("shows")
case object Filters extends IdempotentFns("filters")
case object Updates extends IdempotentFns("updates")
case object Validates extends IdempotentFns("validate_doc_update")

object DesignDocument {
  val PREFIX = "_design/"
  
  def extendId(id: String) = 
    if (id == null || id.length == 0)
      throw new IllegalArgumentException("invalid id entered:" + id)
    else PREFIX + id
}

@BeanInfo
case class DesignDocument(var _id: String, 
  @(JSONProperty @getter)(ignoreIfNull = true, ignore = false) _rev: String, 
  @(JSONTypeHint @field)(value = classOf[View]) views: Map[String, View],
  @(JSONProperty @getter)(ignoreIfNull = true, ignore = false) validate_doc_update: String) {

  if (_id != null) 
    if (!_id.startsWith(DesignDocument.PREFIX))
      _id = DesignDocument.extendId(_id)
  
  var language = "javascript"
  
  private [db] def this() = this(null, null, Map[String, View](), null)
  
  override def toString = {
    "_id = " + _id + " _rev = " + _rev + " language = " + language + " " + 
      (validate_doc_update match {
        case null => ""
        case x => " validate = " + x + " "
      }) + 
      (views match {
        case null => ""
        case v => {
          v.map(e => 
            (e._1.toString + ":" + e._2.toString)).mkString(",")
        }
      }
    )
  }
}
