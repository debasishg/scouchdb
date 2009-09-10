package scouch.db

import java.net.URLEncoder.encode
import sjson.json._
import sjson.json.Implicits._
import dispatch._
import dispatch.json._
import dispatch.json.Js._
import RichRequest._
import DbUtils._

/** Extractors for CouchDB document id and revsion properties.
    Extend with your own document properties. */
trait Id extends Js {
  val _id = Symbol("_id") ? str
  val _rev = Symbol("_rev") ? str
  val _deleted = Symbol("_deleted") ? bool
}
/** Extractors for CouchDB document id and revsion properties.
    Use this object for direct access to Id extractors. */
object Id extends Id

/** Requests for a particular CouchDB host. */
case class Couch(hostname: String, port: Int) extends Request(:/(hostname, port))

/** Factory for a CouchDB Request host with common parameters */
object Couch {
  def apply(): Couch = this("127.0.0.1")
  def apply(hostname: String): Couch = Couch(hostname, 5984)
}

/** Requests on a particular database and CouchDB host. */
case class Db(couch: Couch, name: String) extends Request(couch / name) with Js {
  val all_docs =  this / "_all_docs" ># ('rows ! list andThen { _ map 'id ! str })
  
  /** create a doc from an object with auto id generation */
  def doc[T <: AnyRef](obj: T) = {
    this << JsBean.toJSON(obj) >#  %('id ! str, 'rev ! str)
  }
  
  /** create a doc from an object with auto id generation */
  def doc(obj: JsValue) = {
    this << obj >#  %('id ! str, 'rev ! str)
  }

  val create = this <<< Nil >|
  val delete = DELETE >|
    
  
  /** create multiple docs with auto-generated ids through a single POST. For
      more flexible bulk docs, see @bulkDocs */
  def docs(objs: List[_ <: AnyRef], allOrNothing: Boolean) = {
    val docStr =
      if (allOrNothing) JsValue.toJson(JsValue(Map("docs" -> objs.map(x => Js(JsBean.toJSON(x))), "all_or_nothing" -> true)))
      else JsValue.toJson(JsValue(Map("docs" -> objs.map(x => Js(JsBean.toJSON(x))))))
    
    this / "_bulk_docs" << docStr ># {
      case JsArray(l) => l map %('id ! str, 'rev ! str)
      case s => throw new IllegalArgumentException("invalid data " + s)
    }
  }
  
  /** bulk document creation, updation and deletion */
  def bulkDocs(bulk: List[BulkDocument[_ <: AnyRef]], allOrNothing: Boolean) = {
    val all = bulk map {d =>
      d.obj match {
        case Some(o: AnyRef) =>
          d.options.foldLeft(Js(JsBean.toJSON(o)))((p, q) =>
            q match {
              case Id_(s) => (Id._id << s)(p)
              case Rev_(rev) => (Id._rev << rev)(p)
            }
          )
        case None =>
          d.options.foldLeft(JsObject.apply)((p, q) =>
            q match {
              case Id_(s) => (Id._id << s)(p)
              case Rev_(rev) => (Id._rev << rev)(p)
              case Deleted_(true) => (Id._deleted << true)(p)
            }
          )
      }  
    }
    val docStr =
      if (allOrNothing) JsValue.toJson(JsValue(Map("docs" -> all, "all_or_nothing" -> true)))
      else JsValue.toJson(JsValue(Map("docs" -> all)))
    
    this / "_bulk_docs" << docStr ># {
      case JsArray(l) => l map %('id ! str, 'rev ! str)
      case s => throw new IllegalArgumentException("invalid data " + s)
    }
  }
  
  /** fetch by id, returns a tuple (id, rev) 
      @deprecated use <tt>getRef</tt> instead */
  @deprecated def ref_by_id(id: String) = 
    this / encode(id) ># %(Symbol("_id") ? str, Symbol("_rev") ? str)
  
  /** fetch by id, returns a tuple (id, rev) */
  def getRef(id: String) = 
    this / encode(id) ># %(Symbol("_id") ? str, Symbol("_rev") ? str)
  
  /** fetch by id as an instance of the class <tt>clazz</tt>.
      Returns a Tuple3 of (id, rev, T) */
  import JsBean._
  import scala.reflect.Manifest
  
  /** get an entity of type <tt>T</tt> based on its id. Returns a 
      <tt>Tuple3</tt> of (id, ref, T)
      @deprecated use <tt>get(id)</tt> instead */
  @deprecated def by_id[T](id: String)(implicit m: Manifest[T]) = 
    this / encode(id) ># {
      case s@_ => 
        val (id, ref) = getIdAndRef(s)
        (id, ref, fromJSON(s, Some(m.erasure))) match {  
        case (Some(i), Some(r), x) => (i, r, x.asInstanceOf[T])
        case (_, _, x) => (null, null, x.asInstanceOf[T])
      }
    }
  
  /** get an entity of type <tt>T</tt> based on its id. Returns a 
      <tt>Tuple3</tt> of (id, ref, T) */
  def get[T](id: String)(implicit m: Manifest[T]) = 
    this / encode(id) ># {
      case s@_ => 
        val (id, ref) = getIdAndRef(s)
        (id, ref, fromJSON(s, Some(m.erasure))) match {  
        case (Some(i), Some(r), x) => (i, r, x.asInstanceOf[T])
        case (_, _, x) => (null, null, x.asInstanceOf[T])
      }
    }
  
  /** get an entity of type <tt>T</tt> based on its id and rev. Returns a 
      <tt>Tuple3</tt> of (id, ref, T) 
      @deprecated use <tt>get(id, rev)</tt> instead */
  @deprecated def by_id[T](id: String, rev: String)(implicit m: Manifest[T]) = 
    this / encode(id) <<? Map("rev" -> rev) ># {
      case s@_ => 
        val (id, ref) = getIdAndRef(s)
        (id, ref, fromJSON(s, Some(m.erasure))) match {
        case (Some(i), Some(r), x) => (i, r, x.asInstanceOf[T])
        case (_, _, x) => (null, null, x.asInstanceOf[T])
      } 
    }
  
  /** get an entity of type <tt>T</tt> based on its id and rev. Returns a 
      <tt>Tuple3</tt> of (id, ref, T) */
  def get[T](id: String, rev: String)(implicit m: Manifest[T]) = 
    this / encode(id) <<? Map("rev" -> rev) ># {
      case s@_ => 
        val (id, ref) = getIdAndRef(s)
        (id, ref, fromJSON(s, Some(m.erasure))) match {
        case (Some(i), Some(r), x) => (i, r, x.asInstanceOf[T])
        case (_, _, x) => (null, null, x.asInstanceOf[T])
      } 
    }

  /** fetch the view for the query. The query can be built using the dsl as 
      specified in <tt>ViewQuery</tt> */
  def view(v: Query) = {
    val r = 
      (v.options, v.keys) match {
        case (None, None) => 
          this / v.getViewURIFromName
             
        case (Some(o), None) =>
          this / (v.getViewURIFromName + Options.?(o))
        
        case (None, Some(k)) =>
          this / (v.getViewURIFromName) << JsValue.toJson(JsValue(Map("keys" -> k)))
        
        case (Some(o), Some(k)) =>
          this / (v.getViewURIFromName + Options.?(o)) << JsValue.toJson(JsValue(Map("keys" -> k)))
      } 
    r ># ('rows ! (list ! obj))
  }
  
  /** fetch the view for the query. The query can be built using the dsl as 
      specified in <tt>ViewQuery</tt>. The query returns a list of objects of
      type <tt>clazz</tt> */
  def view[T<:AnyRef](v: Query, clazz: Class[T]) = {
    val r =
      (v.options, v.keys) match {
      case (None, None) => 
        this / v.getViewURIFromName
             
      case (Some(o), None) =>
        this / (v.getViewURIFromName + Options.?(o))
        
      case (None, Some(k)) =>
        this / (v.getViewURIFromName) << JsValue.toJson(JsValue(Map("keys" -> k)))
        
      case (Some(o), Some(k)) =>
        this / (v.getViewURIFromName + Options.?(o)) << JsValue.toJson(JsValue(Map("keys" -> k)))
    } 
    r ># {
      case s => ('rows ! (list ! obj))(s).map {o =>
        if (clazz.isAssignableFrom(classOf[BigDecimal])) {
          val x = Symbol("value") ? num
          val x(x_) = o
          fromJSON(JsNumber(x_), Some(clazz))
        } 
        else if (clazz.isAssignableFrom(classOf[String])) {
          val x = Symbol("value") ? str
          val x(x_) = o
          fromJSON(JsString(x_), Some(clazz))
        } 
        else {
          val x = Symbol("value") ? obj
          val x(x_) = o
          fromJSON(x_, Some(clazz))
        }
      }
    }
  }
  
  /** fetch the adhoc view for the query. The query can be built using the dsl as 
      specified in <tt>AdhocViewQuery</tt> */
  def adhocView(v: AdhocViewQuery) = {
    val r =
      v.options match { // temporary views need to be posted with content type = "application/json"
        case None =>
          this / (v.name) << 
            (JsBean.toJSON(v.functions), "application/json")

        case Some(o) =>
          this / (v.name + "/" + Options.?(o)) << (JsBean.toJSON(v.functions), "application/json")
      }
    r ># ('rows ! (list ! obj))
  }
}


/** Requests on a particular document in a particular database. */
case class Doc(val db: Db, val id: String) extends Request(db / encode(id)) with Js {
  
  /** add an object (bean) to the document */
  def add[T <: AnyRef](obj: T) = {
    this <<< JsBean.toJSON(obj) >|
  }
  
  /** add attachment to a document. None as the <tt>rev</tt> will create a new document
      as well */
  def attach(attachmentId: String, contentType: String, data: Array[byte], rev: Option[String]) = rev match {
    case Some(r) =>
      this / (encode(attachmentId) + Http.?(Map("rev" -> r))) put (data, contentType) >|
    case None =>
      this / encode(attachmentId) put (data, contentType) >|
  }
  
  /** fetch the attachment having id as <tt>attachmentId</tt> */
  def getAttachment(attachmentId: String) = {
    this / encode(attachmentId)
  }
  
  /** delete the attachment specified by the id <tt>attachmentId</tt> for the revision
      <tt>rev</tt> of the current document */
  def deleteAttachment(attachmentId: String, rev: String) = {
    (this / encode(attachmentId) DELETE) <<? Map("rev" -> rev)  >|
  }
  
  /** update the document of specified revision, with the specified object */
  def update[T <: AnyRef](obj: T, r: String) = {
    val js = (Id._rev << r)(Js(JsBean.toJSON(obj)))
    this <<< JsValue.toJson(js) ># {
      case Updated.rev(rev) => (Id._rev << rev)(js)
    }
  }
    
  def update(js: JsValue) = this <<< js ># {
    case Updated.rev(rev) => (Id._rev << rev)(js)
  }
  private object Updated { val rev = 'rev ? str }
  def delete(rev: String) = DELETE <<? Map("rev" -> rev) >|
}

