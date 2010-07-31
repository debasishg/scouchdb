package scouch.db

import org.scalatest.Spec
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import dispatch._
import dispatch.json._
import dispatch.json.JsHttp._

import Options._
import BulkDocument._

@RunWith(classOf[JUnitRunner])
class SCouchDbSpec extends Spec with ShouldMatchers with BeforeAndAfterAll {
  

  val http = new Http
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
  
  describe("Initially the db should not have any document") {
    it("design document count should equal 0") {
      http(test all_docs)
       .filter(_.startsWith("_design") == true)
       .size should equal (0)
    }
    it("non design document count should equal 0") {
      http(test all_docs)
       .filter(_.startsWith("_design") == false)
       .size should equal (0)
    }
  }
  
  describe("Create a design document, query by id and update") {
    val d = DesignDocument("foo", null, Map[String, View](), null)
    val mapfn = "function(doc) { emit(doc.value,doc); }"
    val vi = new View(mapfn, null)
    var revision: String = null
    var nir: (String, String) = null
    val de = Doc(test, d._id)
    
    it("creation should be successful") {
      d._id should equal("_design/foo")
      http(de add d)
      val ir = http(de ># %(Id._id, Id._rev))
      ir._1 should equal(d._id)
    }
    it("design document count should equal 1") {
      http(test all_docs)
       .filter(_.startsWith("_design") == true)
       .size should equal (1)
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
    it("update the document with a view") {
      val doc = DesignDocument(d._id, revision, Map("map" -> vi), null)
      http(de update(doc, revision))
      nir = http(de ># %(Id._id, Id._rev))
      nir._1 should equal(d._id)
      nir._2 should not equal(revision)
    }
    it("design document count should still equal 1") {
      http(test all_docs)
       .filter(_.startsWith("_design") == true)
       .size should equal (1)
    }
    it("re-query by id should fetch the updated document") {
      val ir = http(test getRef d._id)
      ir._1 should equal(d._id)
      val sh = http(test.get[DesignDocument](d._id))
      sh._1 should equal(d._id)
      sh._2 should equal(nir._2)
      sh._3._id should equal(sh._1)
      sh._3._rev should equal(sh._2)
    }
    it("update with incorrect revision should give 409 (conflict in update)") {
      val doc = DesignDocument(d._id, revision, Map("map" -> vi), null) // using same revision as before
      intercept[dispatch.StatusCode] {
        http(de update(doc, revision))
      }
      try {
        http(de update(doc, revision))
      }
      catch {
        case e: dispatch.StatusCode =>
          e.code should equal(409)
      }
    }
    it("update with null revision and same matching id should give 409 (conflict in update)") {
      val doc = DesignDocument(d._id, null, Map("map" -> vi), null) // using null revision 
      try {
        http(de update(doc, revision))
      }
      catch {
        case e: dispatch.StatusCode =>
          e.code should equal(409)
      }
    }
  }
  
  import TestBeans._
  
  describe("Create a document, query by id and update") {
    
    val s = Shop("Shoppers Stop", "refrigerator", 12500)
    val d = Doc(test, "sstop")
    var ir:(String, String) = null
    var ii:(String, String) = null
    var nir:(String, String) = null
    
    it("creation should be successful") {
      http(d add s)
      ir = http(d ># %(Id._id, Id._rev))
      ir._1 should equal("sstop")
    }
    it("query by id should fetch a row") {
      ii = http(test getRef ir._1)
      ii._1 should equal("sstop")
    }
    it("query by id and class should construct an object of that class") {
      val sh = http(test.get[Shop](ir._1))
      sh._1 should equal(ii._1)
      sh._2 should equal(ii._2)
      sh._3.toString should equal(s.toString)
    }
    it("query by id, rev and class should construct an object of that class") {
      val st = http(test.get[Shop](ir._1, ir._2))
      st._1 should equal(ir._1)
      st._2 should equal(ir._2)
      st._3.toString should equal(s.toString)
    }
    it("updating a document should keep the id same and change the revision") {
      val t = Shop("Shoppers Stop", "television", 22500)
      http(d update(t, ir._2))
      nir = http(d ># %(Id._id, Id._rev))
      nir._1 should equal(ir._1)
      nir._2 should not equal(ir._2)
    }
    it("subsequent query by id and class should give changed value for the object's fields") {
      val sh = http(test.get[Shop](ir._1))
      sh._1 should equal(ii._1)
      sh._2 should equal(nir._2)
      sh._3.item should equal("television")
      sh._3.price should equal(22500)
    }
  }
  
  describe("Create a document without explicit id, query by id and update") {
    
    val s = Shop("Capital Electronics", "air-conditioner", 32500)
    var ir:(String, String) = null
    var ii:(String, String) = null
    var nir:(String, String) = null
    var d: Doc = null
    
    it("creation should be successful") {
      val sz = http(test all_docs).size
      ir = http(test doc(s))
      d = Doc(test, ir._1)
      ir._1 should not equal(null)
      ir._2 should not equal(null)
      http(test all_docs).size should equal(sz + 1)
    }
    it("query by id should fetch a row") {
      ii = http(test getRef ir._1)
      ii._1 should equal(ir._1)
    }
    it("query by id and class should construct an object of that class") {
      val sh = http(test.get[Shop](ir._1))
      sh._1 should equal(ii._1)
      sh._2 should equal(ii._2)
      sh._3.toString should equal(s.toString)
      sh._3.store should equal("Capital Electronics")
      sh._3.item should equal("air-conditioner")
      sh._3.price should equal(32500)
    }
    it("query by id, rev and class should construct an object of that class") {
      val st = http(test.get[Shop](ir._1, ir._2))
      st._1 should equal(ir._1)
      st._2 should equal(ir._2)
      st._3.toString should equal(s.toString)
      st._3.store should equal("Capital Electronics")
      st._3.item should equal("air-conditioner")
      st._3.price should equal(32500)
    }
    it("updating a document should keep the id same and change the revision") {
      val t = Shop("Capital Electronics", "air-conditioner", 62500)
      http(d update(t, ir._2))
      nir = http(d ># %(Id._id, Id._rev))
      nir._1 should equal(ir._1)
      nir._2 should not equal(ir._2)
    }
    it("subsequent query by id and class should give changed value for the object's fields") {
      val sh = http(test.get[Shop](ir._1))
      sh._1 should equal(ii._1)
      sh._2 should equal(nir._2)
      sh._3.item should equal("air-conditioner")
      sh._3.price should equal(62500)
    }
  }
  
  describe("Create a complex document from an object having aggregate data member and query by id") {
    
    val addr1 = new Address("10 Market Street", "San Francisco, CA", "94111")
    val addr2 = new Address("3300 Tamarac Drive", "Denver, CO", "98301")
    val c1 = Contact("Debasish Ghosh", Map("primary" -> addr1, "secondary" -> addr2))
    
    val d = Doc(test, "contact_1")
    var ir:(String, String) = null
    var ii:(String, String) = null
    
    it("creation should be successful") {
      http(d add c1)
      ir = http(d ># %(Id._id, Id._rev))
      ir._1 should equal("contact_1")
    }
    it("query by id should fetch a row") {
      ii = http(test getRef ir._1)
      ii._1 should equal("contact_1")
    }
    it("query by id and class should construct an object of that class") {
      val sh = http(test.get[Contact](ir._1))
      sh._1 should equal(ii._1)
      sh._2 should equal(ii._2)
      sh._3.toString should equal(c1.toString)
    }
  }
  
  describe("Create a document from an object whose json property should be diff from bean property and query by id") {
    
    val book = Book(123, "Programming Scala", "1022-09-98762")
    
    val d = Doc(test, "prog_scala")
    var ir:(String, String) = null
    var ii:(String, String) = null
    
    it("creation should be successful") {
      http(d add book)
      ir = http(d ># %(Id._id, Id._rev))
      ir._1 should equal("prog_scala")
    }
    it("query by id should fetch a row") {
      ii = http(test getRef ir._1)
      ii._1 should equal("prog_scala")
    }
    it("query by id and class should construct an object of that class") {
      val sh = http(test.get[Book](ir._1))
      sh._1 should equal(ii._1)
      sh._2 should equal(ii._2)
      sh._3.toString should equal(book.toString)
    }
  }
  
  describe("create records") {
    it("should create new records") {
      val sz = http(test all_docs).size
      val ir1 = http(test doc Js("""{"item":"banana","prices":{"Fresh Mart":1.99,"Price Max":0.79,"Banana Montana":4.22}}"""))
      val ir2 = http(test doc Js("""{"item":"apple","prices":{"Fresh Mart":1.59,"Price Max":5.99,"Apples Express":0.79}}"""))
      val ir3 = http(test doc Js("""{"item":"orange","prices":{"Fresh Mart":1.99,"Price Max":3.19,"Citrus Circus":1.09}}"""))
      http(test all_docs).size should equal(sz + 3)
      println(ir1)
      println(ir2)
      println(ir3)
    }
  }
  
  describe("Create another design document, query by id and update") {
    val d = DesignDocument("lunch", null, Map[String, View](), null)
    val mapfn = "function(doc) {\n    var store, price, key;\n    if (doc.item && doc.prices) {\n        for (store in doc.prices) {\n            price = doc.prices[store];\n            key = [doc.item, price];\n            emit(key, store);\n    }\n  }\n}\n"
    val vi = new View(mapfn, null)
    var revision: String = null
    var nir: (String, String) = null
    val de = Doc(test, d._id)
    
    it("creation should be successful") {
      d._id should equal("_design/lunch")
      http(de add d)
      val ir = http(de ># %(Id._id, Id._rev))
      ir._1 should equal(d._id)
    }
    it("design document count should equal 2") {
      http(test all_docs)
       .filter(_.startsWith("_design") == true)
       .size should equal (2)
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
    it("update the document with a view") {
      val doc = DesignDocument(d._id, revision, Map("least_cost_lunch" -> vi), null)
      http(de update(doc, revision))
      nir = http(de ># %(Id._id, Id._rev))
      nir._1 should equal(d._id)
      nir._2 should not equal(revision)
    }
    it("design document count should still equal 2") {
      http(test all_docs)
       .filter(_.startsWith("_design") == true)
       .size should equal (2)
    }
    it("re-query by id should fetch the updated document") {
      val ir = http(test getRef d._id)
      ir._1 should equal(d._id)
      val sh = http(test.get[DesignDocument](d._id))
      sh._1 should equal(d._id)
      sh._2 should equal(nir._2)
      sh._3._id should equal(sh._1)
      sh._3._rev should equal(sh._2)
    }
    // @fixme
//    it("get all design docs") {
//      val idrev = http(test all_docs).filter(_.startsWith("_design") == true)
//      idrev foreach {ir =>
//        val d = Doc(test, ir)
//      }
//    }
  }
  
  describe("update design document _lunch adding another view big_lunch having map as well as reduce") {
    val mf = 
        """function(doc) {
             var store, price;
             if (doc.item && doc.prices) {
               for (store in doc.prices) {
                 price = doc.prices[store];
                 emit(doc.item, price);
               }
             }
           }"""
      
    val rf = 
      """function(key, values, rereduce) {
           return(sum(values))
         }"""

    val vi = new View(mf, rf)
    
    it("should get design document with id = _design/lunch") {
      val ir = http(test.get[DesignDocument]("_design/lunch"))
      ir._1 should equal("_design/lunch")
      val d = ir._3
      val de = Doc(test, ir._1)
      val doc = DesignDocument(ir._1, ir._2, ir._3.views + ("big_lunch" -> vi), null)
      
      http(de update(doc, ir._2))
      
      val nir = http(de ># %(Id._id, Id._rev))
      nir._1 should equal(ir._1)
      nir._2 should not equal(ir._2)
      
      val new_ir = http(test.get[DesignDocument]("_design/lunch"))
      new_ir._3.views.keySet.size should equal(2)
    }
  }
  
  describe("fetch from view least_cost_lunch") {
    it("should fetch 9 rows from view least_cost_lunch") {
      val ls1 = http(test view(
        Views builder("lunch/least_cost_lunch") build))
      ls1.size should equal(9)
    }
    it("should fetch 1 row for view with option") {
      val ls2 = http(test view(
      Views.builder("lunch/least_cost_lunch")
           .options(optionBuilder key(List("apple", 0.79)) limit(10) build)
           .build))
      ls2.size should equal(1)
    }
    it("should fetch 2 rows for view with key specification") {
      val ls3 = http(test view(
       Views.builder("lunch/least_cost_lunch")
         .keys(List(List("apple", 0.79), List("banana", 0.79)))
         .build))
      ls3.size should equal(2)
    }
    it("should fetch 2 rows for view with option and key specification") {
      val ls4 = http(test view(
       Views.builder("lunch/least_cost_lunch")
         .options(optionBuilder descending(true) limit(10) build)
         .keys(List(List("apple", 0.79), List("banana", 0.79)))
         .build))
      ls4.size should equal(2)
    }
  }
  
  describe("fetch from view big_lunch") {
    it("should fetch 1 row from view big_lunch") {
      val ls1 = http(test view(
        Views builder("lunch/big_lunch") build))
      ls1.size should equal(1)
    }
    it("should also fetch 1 row from view big_lunch") {
      val ls1 = http(test view(
        Views.builder("lunch/big_lunch").options(optionBuilder group(true) limit(10) build).build))
      ls1.size should equal(3)
    }
  }
  
  describe("fetch from temporary views") {
    it("should fetch 3 rows with group option and 1 row without group option") {
      val mf = 
        """function(doc) {
             var store, price;
             if (doc.item && doc.prices) {
               for (store in doc.prices) {
                 price = doc.prices[store];
                 emit(doc.item, price);
               }
             }
           }"""
      
      val rf = 
        """function(key, values, rereduce) {
             return(sum(values))
           }"""
      
      // with grouping
      val aq = 
        Views.adhocBuilder(View(mf, rf))
             .options(optionBuilder group(true) build)
             .build
      val s = http(
        test adhocView(aq))
      s.size should equal(3)
      
      // without grouping
      val aq_1 = 
        Views.adhocBuilder(View(mf, rf))
             .build
      val s_1 = http(
        test adhocView(aq_1))
      s_1.size should equal(1)
    }
  }
  
  describe("all_docs with view query options") {
    it("should fetch all docs") {
      val ls1 = http(test view(
        Views builder("_all_docs") build))
      ls1.size should equal(9)
    }
    it("include_docs should also fetch documents as well") {
      val ls1 = http(test view(
        Views.builder("_all_docs").options(optionBuilder includeDocs(true) build).build))
      ls1.map { j =>
        val d = ('doc ? obj)
        val d(d_) = j
        d_
      }.size should equal(9)
    }
  }
  
  describe("create a document and make an attachment") {
    val att = "The quick brown fox jumps over the lazy dog."
    
    val s = Shop("Sears", "refrigerator", 12500)
    val d = Doc(test, "sears")
    var ir:(String, String) = null
    var ii:(String, String) = null
    
    it("document creation should be successful") {
      http(d add s)
      ir = http(d ># %(Id._id, Id._rev))
      ir._1 should equal("sears")
    }
    it("query by id should fetch a row") {
      ii = http(test getRef ir._1)
      ii._1 should equal("sears")
    }
    it("sticking an attachment should be successful") {
      http(d attach("foo", "text/plain", att.getBytes, Some(ii._2)))
    }
    it("retrieving the attachment should equal to att") {
      val air = http(d ># %(Id._id, Id._rev))
      air._1 should equal("sears")
      http(d.getAttachment("foo") as_str) should equal(att)
    }
  }
  
  describe("create an attachment on a non-existing document") {
    val att = "The quick brown fox jumps over the lazy dog."
    val new_att = "Hedge hog and a sly fox"
    
    val s = Shop("cc", "refrigerator", 12500)
    val d = Doc(test, "cc")
    var ir:(String, String) = null
    var ii:(String, String) = null
    var nir:(String, String) = null
    var air:(String, String) = null
    
    it("d has not yet been added, still attachment should go through and create a document as well") {
      http(d attach("foo", "text/plain", att.getBytes, None))
      air = http(d ># %(Id._id, Id._rev))
      air._1 should equal("cc")
      http(d.getAttachment("foo") as_str) should equal(att)
    }
    it("query by id should fetch a row") {
      ii = http(test getRef "cc")
      ii._1 should equal("cc")
    }
    it("change an attachment") {
      http(d attach("foo", "text/plain", new_att.getBytes, Some(ii._2)))
      air = http(d ># %(Id._id, Id._rev))
      air._1 should equal("cc")
      http(d.getAttachment("foo") as_str) should equal(new_att)
    }
    it("add another attachment") {
      http(d attach("bar", "text/plain", att.getBytes, Some(air._2)))
      air = http(d ># %(Id._id, Id._rev))
      air._1 should equal("cc")
      http(d.getAttachment("foo") as_str) should equal(new_att)
      http(d.getAttachment("bar") as_str) should equal(att)
    }
    it("delete new_att") {
      http(d deleteAttachment("foo", air._2))
      air = http(d ># %(Id._id, Id._rev))
      air._1 should equal("cc")
      http(d.getAttachment("bar") as_str) should equal(att)

      http.x(d.getAttachment("foo")) {
        case (c, res, e) => c
      } should equal(404)
    }
  }
  
  describe("bulk updates of documents") {
    it("should create 3 documents with 1 post") {
      val cnt = http(test all_docs).filter(_.startsWith("_design") == false).size 
      
      val s1 = Shop("cc", "refrigerator", 12500)
      val s2 = Shop("best buy", "macpro", 1500)
      val a1 = Address("Survey Park", "Kolkata", "700075")
      val a2 = Address("Salt Lake", "Kolkata", "700091")
      
      http(test docs(List(s1, s2, a1, a2), false)).size should equal(4)
      http(test all_docs).filter(_.startsWith("_design") == false).size should equal(cnt + 4)
    }
    it("should insert 2 new documents, update 1 existing document and delete 1 - all in 1 post") {
      val sz = http(test all_docs).filter(_.startsWith("_design") == false).size
      val s = Shop("Shoppers Stop", "refrigerator", 12500)
      val d = Doc(test, "ss")
      
      val t = Address("Monroe Street", "Denver, CO", "987651")
      val ad = Doc(test, "add1")
      
      var ir:(String, String) = null
      var ir1:(String, String) = null
    
      http(d add s)
      ir = http(d ># %(Id._id, Id._rev))
      ir._1 should equal("ss")
      
      http(ad add t)
      ir1 = http(ad ># %(Id._id, Id._rev))
      ir1._1 should equal("add1")
      
      val s1 = Shop("cc", "refrigerator", 12500)
      val s2 = Shop("best buy", "macpro", 1500)
      val a1 = Address("Survey Park", "Kolkata", "700075")
      
      val d1 = bulkBuilder(Some(s1)).id("a").build 
      val d2 = bulkBuilder(Some(s2)).id("b").build
      val d3 = bulkBuilder(Some(s)).id("ss").rev(ir._2).build
      val d4 = bulkBuilder(None).id("add1").rev(ir1._2).deleted(true).build
      http(test bulkDocs(List(d1, d2, d3, d4), false)).size should equal(4)
      http(test all_docs).filter(_.startsWith("_design") == false).size should equal(sz + 3)
    }
  }
  
  describe("Create a document, query by id and requery for conditionalGet") {
    
    val s = Shop("Bloomingdale", "refrigerator", 12500)
    val d = Doc(test, "bdl")
    var ir:(String, String) = null
    var ii:(String, String) = null
    var nir:(String, String) = null
    
    it("creation should be successful") {
      http(d add s)
      ir = http(d ># %(Id._id, Id._rev))
      ir._1 should equal("bdl")
    }
    it("query by id should fetch a row") {
      ii = http(test getRef ir._1)
      ii._1 should equal("bdl")
    }
    it("query by id and class should construct an object of that class") {
      val sh = http(test.get[Shop](ir._1))
      sh._1 should equal(ii._1)
      sh._2 should equal(ii._2)
      sh._3.toString should equal(s.toString)
    }
    it("query by id, rev and class should construct an object of that class") {
      val st = http(test.get[Shop](ir._1, ir._2))
      st._1 should equal(ir._1)
      st._2 should equal(ir._2)
      st._3.toString should equal(s.toString)
    }
    it("conditionalGet should give 304") {
      try {
        http(test.conditionalGet[Shop](ir._1, ir._2))
      }
      catch {
        case e: dispatch.StatusCode =>
          e.code should equal(304)
      }
    }
    it("updating a document should change the revision") {
      val t = Shop("Bloomingdale", "television", 22500)
      http(d update(t, ir._2))
      nir = http(d ># %(Id._id, Id._rev))
      nir._1 should equal(ir._1)
      nir._2 should not equal(ir._2)
    }
    it("subsequent conditionalGet should refetch and not throw") {
      val sh = http(test.conditionalGet[Shop](ir._1, ir._2))
      sh._1 should equal(ir._1)
      sh._2 should equal(nir._2)
      sh._3.item should equal("television")
      sh._3.price should equal(22500)
    }
  }

  describe("Create a design document with pass thru validation function") {
    val all_pass = "function(newDoc, oldDoc, userCtx) {}"  // all valid
    val d = DesignDocument("foo_valid", null, Map[String, View](), all_pass)
    val de = Doc(test, d._id)
    
    it("creation should be successful") {
      d._id should equal("_design/foo_valid")
      http(de add d)
      val ir = http(de ># %(Id._id, Id._rev))
      ir._1 should equal(d._id)
    }
    it("creation of a document should be successful") {
      val s = Shop("Crossword", "The C Programming Language", 200)
      val d = Doc(test, "crswd")
      http(d add s)
      val ir = http(d ># %(Id._id, Id._rev))
      ir._1 should equal("crswd")
    }
  }
  
  describe("Create a design document with all-fail validation function") {
    val all_fail = "function(newDoc, oldDoc, userCtx) {throw({forbidden : 'no way'});}"
    val d = DesignDocument("foo_invalid", null, Map[String, View](), all_fail)
    val de = Doc(test, d._id)
    
    it("creation should be successful") {
      d._id should equal("_design/foo_invalid")
      http(de add d)
      val ir = http(de ># %(Id._id, Id._rev))
      ir._1 should equal(d._id)
    }
    it("creation of a document should not be successful") {
      val s = Shop("Crossword", "The C Programming Language", 200)
      val d = Doc(test, "crswd1")
      
      intercept[dispatch.StatusCode] {
        http(d add s)
      }
      try {
        http(d add s)
      }
      catch {
        case e: dispatch.StatusCode =>
          e.code should equal(403)
      }
    }
  }
}
