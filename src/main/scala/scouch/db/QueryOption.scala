package scouch.db

import dispatch.json._

trait QueryOption[T] {
  val key: String
  val value: T
}

case class Key(value: JsValue) extends QueryOption[JsValue] {
  val key = "key"
}

case class StartKey(value: JsValue) extends QueryOption[JsValue] {
  val key = "startkey"
}

case class StartKeyDocId(value: String) extends QueryOption[String] {
  val key = "startKey_docid"
}

case class EndKey(value: JsValue) extends QueryOption[JsValue] {
  val key = "endkey"
}

case class EndKeyDocId(value: String) extends QueryOption[String] {
  val key = "endKey_docid"
}

case class Limit(value: Int) extends QueryOption[Int] {
  val key = "limit"
}

case class Update(value: Boolean) extends QueryOption[Boolean] {
  val key = "update"
}

case class Descending(value: Boolean) extends QueryOption[Boolean] {
  val key = "descending"
}

case class Skip(value: Int) extends QueryOption[Int] {
  val key = "skip"
}

case class Group(value: Boolean) extends QueryOption[Boolean] {
  val key = "group"
}

case class GroupLevel(value: Int) extends QueryOption[Int] {
  val key = "group_level"
}

case class Reduce(value: Boolean) extends QueryOption[Boolean] {
  val key = "reduce"
}

case class IncludeDocs(value: Boolean) extends QueryOption[Boolean] {
  val key = "include_docs"
}

case class Stale(value: Boolean) extends QueryOption[Boolean] {
  val key = "stale"
}


import org.apache.http.client.utils.URLEncodedUtils
import org.apache.http.message.BasicNameValuePair
import org.apache.http.protocol.HTTP

object Options {
  
  class OptionBuilder(options: List[QueryOption[_]]) {
    def key(value: Any) = new OptionBuilder(Key(JsValue(value)) :: options)
    def startKey(value: Any) = new OptionBuilder(StartKey(JsValue(value)) :: options) 
    def startKeyDocId(value: String) = new OptionBuilder(StartKeyDocId(value) :: options) 
    def endKey(value: Any) = new OptionBuilder(EndKey(JsValue(value)) :: options) 
    def endKeyDocId(value: String) = new OptionBuilder(EndKeyDocId(value) :: options) 
    def limit(value: Int) = new OptionBuilder(Limit(value) :: options) 
    def update(value: Boolean) = new OptionBuilder(Update(value) :: options) 
    def descending(value: Boolean) = new OptionBuilder(Descending(value) :: options) 
    def skip(value: Int) = new OptionBuilder(Skip(value) :: options) 
    def group(value: Boolean) = new OptionBuilder(Group(value) :: options) 
    def groupLevel(value: Int) = new OptionBuilder(GroupLevel(value) :: options) 
    def reduce(value: Boolean) = new OptionBuilder(Reduce(value) :: options) 
    def includeDocs(value: Boolean) = new OptionBuilder(IncludeDocs(value) :: options) 
    def stale(value: Boolean) = new OptionBuilder(Stale(value) :: options) 
    
    def build = options
  }
  
  def optionBuilder = new OptionBuilder(List())
  
  def ?(values: List[QueryOption[_]]) = if (values == null || values.isEmpty) "" else
    "?" + URLEncodedUtils.format(list2ee(values), HTTP.UTF_8)
  
  private def list2ee(values: List[QueryOption[_]]) = 
    new java.util.ArrayList[BasicNameValuePair](values.size) {
      values.foreach {v => add(new BasicNameValuePair(v.key, v.value.toString))}
    }
}
