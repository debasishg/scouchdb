package scouch.db

import org.scalatest.Spec
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import dispatch._
import dispatch.json._
import dispatch.json.JsHttp._
import sjson.json._
import Options._

import scouch.db.TestBeans._

@RunWith(classOf[JUnitRunner])
class ScalaValidationSpec extends Spec with ShouldMatchers with BeforeAndAfterEach {
  
  val http = new Http
  val test = Db(Couch(), "test")
  
//  val test = Db(Couch("localhost", "jchris", "secretpass"), "test") // these tests expect CouchDB to be running at 127.0.0.1 on port 5984

  override def beforeEach {
    http(test.create)
    http(test as_str) should startWith("""{"db_name":"test","doc_count":0""")
    println("** created database")
  }
  
  override def afterEach {
    http(test.delete)
    (http x test) { (status, _, _) => status } should equal (404)
    println("** destroyed database")
  }
  
  private def deleteAllDocs {
    http(test all_docs)
    .map(id => http(test getRef id))
      .foreach{ir => http(Doc(test, ir._1).delete(ir._2))}
  }
  
  describe("Create a document with pass thru validation function in design document") {
    
    it("creation should be successful") {
      val vfn = """(ndoc: dispatch.json.JsValue,
        odoc: dispatch.json.JsValue, req: Any) => {}"""

      val d = DesignDocument("foo_1", null, Map[String, View](), vfn)
      d.language = "scala"

      val de = Doc(test, d._id)

      d._id should equal("_design/foo_1")
      http(de add d)
      val ir = http(de ># %(Id._id, Id._rev))
      ir._1 should equal(d._id)
      http(test doc Js("""{"item":"oranges","prices":{"Fresh Mart":1.99,"Price Max":3.19,"Citrus Circus":1.09}}"""))
    }
  }

  /**
  describe("Add a validation function and check specific validation of documents") {

    it("should allow creation of televisions but not air-conditioners") {
      val vfn = """(ndoc: dispatch.json.JsValue,
        odoc: dispatch.json.JsValue, req: Any) => {
        import scouch.db.TestBeans._
        import scouch.db.ViewServerUtils._
        val s = sjson.json.JsBean.fromJSON(ndoc, Some(classOf[Shop]))
        if (s.asInstanceOf[Shop].item == "air-conditioner") throw new ValidationException("Cannot allow")
      }"""
      
      val d = DesignDocument("foo_2", null, Map[String, View](), vfn)
      d.language = "scala"
      val de = Doc(test, d._id)
      d._id should equal("_design/foo_2")
      http(de add d)
      val ir = http(de ># %(Id._id, Id._rev))
      ir._1 should equal(d._id)
      val s1 = Shop("Capital Electronics", "television", 62500)
      val d1 = Doc(test, "celec")
      http(d1 add s1)
      val d1r = http(d1 ># %(Id._id, Id._rev))
      d1r._1 should equal("celec")
      val s2 = Shop("Capital Electronics", "air-conditioner", 32500)
      val d2 = Doc(test, "cel")
      try {
        http(d2 add s2)
      }
      catch {
        case e: dispatch.StatusCode =>
          e.code should equal(403)
      }
    }
  }

  describe("Update an existing document after adding an all-fail validation function to design document") {

    it("updation of an existing document should fail after adding an all-fail validation function to design document") {
      val d = DesignDocument("foo_3", null, Map[String, View](), null)
      d.language = "scala"
      val de = Doc(test, d._id)
      d._id should equal("_design/foo_3")
      var ir:(String, String) = null
      var orangeDoc:(String, String) = null
      var docJs: JsValue = null
    
      // creation of design document should be successful
      http(de add d)
      ir = http(de ># %(Id._id, Id._rev))
      ir._1 should equal(d._id)
      
      // creation of a document should be successful
      docJs = Js("""{"item":"oranges","prices":{"Fresh Mart":1.99,"Price Max":3.19,"Citrus Circus":1.09}}""")
      orangeDoc = http(test doc docJs)
      
      val s = Shop("Capital Electronics", "refrigerator", 32500)
      val (id, rev) = http(test doc(s))
      val d1 = Doc(test, id)

      // update design document with validation function
      val vfn = """(ndoc: dispatch.json.JsValue,
        odoc: dispatch.json.JsValue, req: Any) => { 
          import scouch.db.ViewServerUtils._
          throw new ValidationException("Cannot update")
        }"""

      val ddoc = DesignDocument("foo_3", ir._2, Map[String, View](), vfn)
      ddoc.language = "scala"
      http(de update(ddoc, ir._2))
      val ndir = http(de ># %(Id._id, Id._rev))
      ndir._1 should equal(ir._1)
      ndir._2 should not equal(ir._2)

      // need to update the document
      intercept[dispatch.StatusCode] {
        val t = Shop("Capital Electronics", "refrigerator", 62500)
        http(d1 update(t, rev))
        val nir = http(d1 ># %(Id._id, Id._rev))
        nir._1 should equal(id)
        nir._2 should not equal(rev)
      }
      try {
        val t = Shop("Capital Electronics", "refrigerator", 62500)
        http(d1 update(t, rev))
        val nir = http(d1 ># %(Id._id, Id._rev))
        nir._1 should equal(id)
        nir._2 should not equal(rev)
      }
      catch {
        case e: dispatch.StatusCode =>
          e.code should equal(403)
      }
    }
  }

  describe("Create a design document with all fail validation function") {
    
    it("creation of a document should fail") {
      
      val vfn = """(ndoc: dispatch.json.JsValue,
        odoc: dispatch.json.JsValue, req: Any) => { throw new scouch.db.ViewServerUtils.ValidationException("Cannot update"); }"""

      val d = DesignDocument("foo_invalid", null, Map[String, View](), vfn)
      d.language = "scala"

      val de = Doc(test, d._id)

      d._id should equal("_design/foo_invalid")
      http(de add d)
      val ir = http(de ># %(Id._id, Id._rev))
      ir._1 should equal(d._id)
      
      intercept[dispatch.StatusCode] {
        http(test doc Js("""{"item":"peaches","prices":{"Fresh Mart":1.99,"Price Max":3.19,"Citrus Circus":1.09}}"""))
      }
      try {
        http(test doc Js("""{"item":"peaches","prices":{"Fresh Mart":1.99,"Price Max":3.19,"Citrus Circus":1.09}}"""))
      }
      catch {
        case e: dispatch.StatusCode =>
          e.code should equal(403)
      }
    }
  }
  
  describe("Add a validation function for checking mandatory fields of documents") {

    it("should not allow creation of s2 with null store name") {
      val vfn = """(ndoc: dispatch.json.JsValue,
        odoc: dispatch.json.JsValue, req: Any) => {

        import scouch.db.ViewServerUtils._
        def require(fields: List[String]) = {
          fields.foreach(f =>
            if (f == null || f == "null" || f.length == 0)
              throw new ValidationException("Cannot allow"))
        }

        import scouch.db.TestBeans._
        val s = sjson.json.JsBean.fromJSON(ndoc, Some(classOf[Shop]))
        require(List(s.store, s.item))
      }"""

      val d = DesignDocument("foo_2", null, Map[String, View](), vfn)
      d.language = "scala"
      val de = Doc(test, d._id)
      d._id should equal("_design/foo_2")
      http(de add d)
      val ir = http(de ># %(Id._id, Id._rev))
      ir._1 should equal(d._id)
      
      val s1 = Shop("Shoppers' Stop", "television", 62500)
      http(test doc(s1))
      
      val s2 = Shop(null, "air-conditioner", 32500)
      intercept[dispatch.StatusCode] {
        http(test doc(s2))
      }
      try {
        http(test doc(s2))
      }
      catch {
        case e: dispatch.StatusCode =>
          e.code should equal(403)
      }
    }
  }
  
  describe("Update an existing document by trying to change a logically immutable field (e.g. creation date)") {

    it("updation of shop should fail on change of store name") {
      val d = DesignDocument("foo_3", null, Map[String, View](), null)
      d.language = "scala"
      val de = Doc(test, d._id)
      d._id should equal("_design/foo_3")
      var ir:(String, String) = null
      var orangeDoc:(String, String) = null
      var docJs: JsValue = null
    
      // creation of design document should be successful
      http(de add d)
      ir = http(de ># %(Id._id, Id._rev))
      ir._1 should equal(d._id)
      
      val s = Shop("Capital Electronics", "refrigerator", 32500)
      val (id, rev) = http(test doc(s))
      val d1 = Doc(test, id)

      // update design document with validation function
      val vfn = """(ndoc: dispatch.json.JsValue,
        odoc: dispatch.json.JsValue, req: Any) => {

        def unchanged(field: String) = {
          import dispatch.json._
          import Js._
          import scouch.db.ViewServerUtils._

          val s = Symbol(field) ? str
          val s(_s) = odoc
          val s(_s1) = ndoc
          if (odoc != null && _s != _s1) 
            throw new ValidationException("Field cannot be changed: " + field)
        }
        unchanged("item")
        unchanged("store")
      }"""

      val ddoc = DesignDocument("foo_3", ir._2, Map[String, View](), vfn)
      ddoc.language = "scala"
      http(de update(ddoc, ir._2))
      val ndir = http(de ># %(Id._id, Id._rev))
      ndir._1 should equal(ir._1)
      ndir._2 should not equal(ir._2)

      // try to update the document: should fail with 403
      try {
        val t = Shop("Sears", "refrigerator", 62500)
        http(d1 update(t, rev))
        val nir = http(d1 ># %(Id._id, Id._rev))
        nir._1 should equal(id)
        nir._2 should not equal(rev)
      }
      catch {
        case e: dispatch.StatusCode =>
          e.code should equal(403)
      }
    }
  }
**/
}
