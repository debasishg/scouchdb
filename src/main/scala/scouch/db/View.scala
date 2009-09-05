package scouch.db

import scala.reflect._

import sjson.json._

@BeanInfo
case class View(
  @JSONProperty {val ignoreIfNull = true}
  val map: String, 
  
  @JSONProperty {val ignoreIfNull = true}
  val reduce: String) {
  
  private [db] def this() = this(null, null)
  
  override def toString = 
    "map: " + map + " reduce: " + reduce
}
