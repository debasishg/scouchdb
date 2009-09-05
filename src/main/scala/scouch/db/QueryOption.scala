package scouch.db

import dispatch.json._

case class QueryOption[T](key: String, value: T)

case class Key(override val value: JsValue) extends QueryOption("key", value)

case class StartKey(override val value: JsValue) extends QueryOption("startkey", value)

case class StartKeyDocId(override val value: String) extends QueryOption("startkey_docid", value)

case class EndKey(override val value: JsValue) extends QueryOption("endkey", value)

case class EndKeyDocId(override val value: String) extends QueryOption("endkey_docid", value)

case class Limit(override val value: Int) extends QueryOption("limit", value)

case class Update(override val value: Boolean) extends QueryOption("update", value)

case class Descending(override val value: Boolean) extends QueryOption("descending", value)

case class Skip(override val value: Int) extends QueryOption("skip", value)

case class Group(override val value: Boolean) extends QueryOption("group", value)

case class GroupLevel(override val value: Int) extends QueryOption("group_level", value)

case class Reduce(override val value: Boolean) extends QueryOption("reduce", value)

case class IncludeDocs(override val value: Boolean) extends QueryOption("include_docs", value)

case class Stale(override val value: Boolean) extends QueryOption("stale", value)


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
