package scouch.db

import sjson.json._ 
import scala.reflect._

object DesignDocument {
  val PREFIX = "_design/"
  
  def extendId(id: String) = 
    if (id == null || id.length == 0)
      throw new IllegalArgumentException("invalid id entered:" + id)
    else PREFIX + id
}

@BeanInfo
case class DesignDocument(var _id: String, 
                          
                          @JSONProperty("") {val ignoreIfNull = true, val ignore = false } 
                          _rev: String, 
                          
                          @OptionTypeHint(classOf[Map[_,_]])
                          @JSONTypeHint(classOf[View])
                          views: Map[String, View],
                          
                          @JSONProperty("") { val ignoreIfNull = true, val ignore = false }
                          validate_doc_update: String) {
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
