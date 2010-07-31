package scouch.db

import scala.reflect._
import scala.annotation.target._

import sjson.json._

@BeanInfo
case class View(
  @(JSONProperty @getter)(ignoreIfNull = true)
  val map: String, 
  
  @(JSONProperty @getter)(ignoreIfNull = true)
  val reduce: String) {
  
  private [db] def this() = this(null, null)
  
  override def toString = 
    "map: " + map + " reduce: " + reduce
}
