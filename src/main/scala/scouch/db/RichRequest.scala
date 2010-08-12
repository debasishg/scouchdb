package scouch.db

import org.apache.http._
import org.apache.http.client._
import org.apache.http.client.methods._
import org.apache.http.entity.{StringEntity, ByteArrayEntity}
import org.apache.http.params.{HttpProtocolParams, BasicHttpParams}
import org.apache.http.protocol.HTTP

import dispatch._

class Post(val value: Any, val contentType: Option[String]) extends HttpPost {
  contentType match {
    case None => this setEntity new StringEntity(value.toString, HTTP.UTF_8)
    case Some(str) =>
      val e = new StringEntity(value.toString)
      e setContentType(str)
      this setEntity e
  }
}

object RichRequest {
  class RichRequest(req: Request) {
    /** Post the given body and return response wrapper. (new request, mimics) */
    def << (body: Any) = req.next { 
      Request.mimic(new Post(body, None))_ 
    }
    
    /** Post the given body with the specified contentType and return response wrapper. (new request, mimics) */
    def << (body: Any, contentType: String) = req.next { Request.mimic(new Post(body, Some(contentType)))_ }
    
    /** Put the given object and return response wrapper. (new request, mimics) */
    def put (body: Array[Byte], contentType: String) = req.next {
      val m = new HttpPut
      val entity = new ByteArrayEntity(body)
      entity.setContentType(contentType)
      m setEntity entity
      HttpProtocolParams.setUseExpectContinue(m.getParams, false)
      Request.mimic(m)_
    }
  }
  implicit def enrichRequest(req: Request) = new RichRequest(req)
}


