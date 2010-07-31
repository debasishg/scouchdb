package scouch.db

import org.scalatest.Spec
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import dispatch._
import dispatch.json._
import dispatch.json.JsHttp._
import sjson.json._
import Options._

@RunWith(classOf[JUnitRunner])
class ScalaViewServerSpec  extends Spec with ShouldMatchers with BeforeAndAfterAll {
  
  val http = new Http
//  val test = Db(Couch("localhost", "jchris", "secretpass"), "test") // these tests expect CouchDB to be running at 127.0.0.1 on port 5984
  val test = Db(Couch(), "test") // these tests expect CouchDB to be running at 127.0.0.1 on port 5984

  override def beforeAll {
    http(test.create)
    http(test as_str) should startWith("""{"db_name":"test","doc_count":0""")
    println("** created database")
  }
  
  override def afterAll {
    http(test.delete)
    (http x test) { (status, _, _) => status } should equal (404)
    println("** destroyed database")
  }
  
  describe("create records") {
    it("should create new records") {
      http(test doc Js("""{"item":"banana","prices":{"Fresh Mart":1.99,"Price Max":0.79,"Banana Montana":4.22}}"""))
      http(test doc Js("""{"item":"apple","prices":{"Fresh Mart":1.59,"Price Max":5.99,"Apples Express":0.79}}"""))
      http(test doc Js("""{"item":"orange","prices":{"Fresh Mart":1.99,"Price Max":3.19,"Citrus Circus":1.09}}"""))
    }
  }
  
  describe("Create a design document, for scala view") {
    val d = DesignDocument("power", null, Map[String, View](), null)
    d.language = "scala"
    val mapfn1 = """(doc: dispatch.json.JsValue) => {
          val it = sjson.json.JsBean.fromJSON(doc, Some(classOf[scouch.db.TestBeans.Item_1])); 
          for (st <- it.prices)
            yield(List(it.item, st._2))
        }"""
    
    val mapfn2 = """(doc: dispatch.json.JsValue) => {
          import dispatch.json.Js._; 
          val x = Symbol("item") ? dispatch.json.Js.str;
          val x(x_) = doc; 
          val i = Symbol("_id") ? dispatch.json.Js.str;
          val i(i_) = doc;
          List(List(i_, x_)) ;
        }"""

    val vi_1 = new View(mapfn1, null)
    val vi_2 = new View(mapfn2, null)
                        
    var revision: String = null
    var nir: (String, String) = null
    val de = Doc(test, d._id)
    
    it("creation should be successful") {
      d._id should equal("_design/power")
      http(de add d)
      val ir = http(de ># %(Id._id, Id._rev))
      ir._1 should equal(d._id)
    }
    it("query by id should fetch the document") {
      val ir = http(test getRef d._id)
      ir._1 should equal(d._id)
      val sh = http(test.get[DesignDocument](d._id))
      sh._1 should equal(d._id)
      sh._2 should equal(ir._2)
      sh._3._id should equal(sh._1)
      sh._3._rev should equal(sh._2)
      revision = sh._2
    }
    it("update the document with 2 views") {
      val doc = DesignDocument(d._id, revision, Map("power_lunch" -> vi_1, "mega_lunch" -> vi_2), null)
      doc.language = "scala"
      http(de update(doc, revision))
      nir = http(de ># %(Id._id, Id._rev))
      nir._1 should equal(d._id)
      nir._2 should not equal(revision)
    }
    it("should fetch 9 rows from view power_lunch") {
      val ls1 = http(test view(
        Views builder("power/power_lunch") build))
      println(ls1)
      ls1.size should equal(9)
    }
    it("should fetch 3 rows from view mega_lunch") {
      val ls1 = http(test view(
        Views builder("power/mega_lunch") build))
      println(ls1)
      ls1.size should equal(3)
    }
  }
  
  describe("Create a design document, for scala view with map and reduce") {
    val d = DesignDocument("big", null, Map[String, View](), null)
    d.language = "scala"
    val mapfn1 = """(doc: dispatch.json.JsValue) => {
          val it = sjson.json.JsBean.fromJSON(doc, Some(classOf[scouch.db.TestBeans.Item_1])); 
          for (st <- it.prices)
            yield(List(it.item, st._2))
        }"""
    
    val redfn1 = """(key: List[(String, String)], values: List[dispatch.json.JsNumber], rereduce: Boolean) => {
          values.foldLeft(BigDecimal(0.00))((s, f) => s + (f match { case dispatch.json.JsNumber(n) => n }))
        }"""
    
    val vi_1 = new View(mapfn1, redfn1)
                        
    var revision: String = null
    var nir: (String, String) = null
    val de = Doc(test, d._id)
    
    it("creation should be successful") {
      d._id should equal("_design/big")
      http(de add d)
      val ir = http(de ># %(Id._id, Id._rev))
      ir._1 should equal(d._id)
    }
    it("query by id should fetch the document") {
      val ir = http(test getRef d._id)
      ir._1 should equal(d._id)
      val sh = http(test.get[DesignDocument](d._id))
      sh._1 should equal(d._id)
      sh._2 should equal(ir._2)
      sh._3._id should equal(sh._1)
      sh._3._rev should equal(sh._2)
      revision = sh._2
    }
    it("update the document with the view") {
      val doc = DesignDocument(d._id, revision, Map("big_lunch" -> vi_1), null)
      doc.language = "scala"
      http(de update(doc, revision))
      nir = http(de ># %(Id._id, Id._rev))
      nir._1 should equal(d._id)
      nir._2 should not equal(revision)
    }
    it("should fetch 1 row from view big_lunch") {
      val ls1 = http(test view(
        Views.builder("big/big_lunch")
             .build))
      println(ls1)
      ls1.size should equal(1)
    }
    it("should fetch 3 rows from view big_lunch") {
      val ls1 = http(test view(
        Views.builder("big/big_lunch")
             .options(optionBuilder group(true) build)
             .build))
      println(ls1)
      ls1.size should equal(3)
    }
  }
}
