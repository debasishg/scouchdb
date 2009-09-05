package scouch.db

import dispatch.json._
import dispatch.json.Js._

object DbUtils {
  def getIdAndRef(js: JsValue) = {
    try {
      val id = Symbol("_id") ? str
      val id(id_) = js
    
      val rev = Symbol("_rev") ? str
      val rev(rev_) = js
      
      (Some(id_), Some(rev_))
    }
    catch {
      case me: scala.MatchError => {
        (None, None)
      }
    }
  }
}
