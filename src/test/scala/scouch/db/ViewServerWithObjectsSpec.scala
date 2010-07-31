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

import TestBeans._

@RunWith(classOf[JUnitRunner])
class ViewServerWithObjectsSpec extends Spec with ShouldMatchers with BeforeAndAfterAll {
  
  val http = new Http
  val carDb = Db(Couch(), "car_inventory_db") // these tests expect CouchDB to be running at 127.0.0.1 on port 5984

  override def beforeAll {
    http(carDb.create)
    http(carDb as_str) should startWith("""{"db_name":"car_inventory_db","doc_count":0""")
    println("** created database")
  }
  
  override def afterAll {
    http(carDb.delete)
    (http x carDb) { (status, _, _) => status } should equal (404)
    println("** destroyed database")
  }
  
  describe("create records") {
    it("should create new records") {
      def newCar(carId : String) = Doc(carDb, carId)

      // add
      http(newCar("honda_civic_new_red") add CarSaleItem("Honda", "Civic", 16000, "New", "Red"))
      http(newCar("tesla_roadster_new_orange") add CarSaleItem("Tesla", "Roadster", 90000, "New", "Orange"))
      http(newCar("bmw_mini_used_redwhiteblue") add CarSaleItem("BMW", "Mini", 12500, "Used", "Red/White/Blue"))
      http(newCar("ford_mustang_used_silver") add CarSaleItem("Ford", "Mustang", 14500, "Used", "Silver"))
      http(newCar("geo_metro_used_red") add CarSaleItem("Geo", "Metro", 7500, "Used", "Red"))
    }
  }
  
  describe("Create, query, view : only maps") {
    it("should identify red cars") {
       
      // query by id to get back the object
      // returns a tuple3 of (id, rev, object)
      val (id, rev, item) = http(carDb.get[CarSaleItem]("bmw_mini_used_redwhiteblue"))
      
      id should equal("bmw_mini_used_redwhiteblue")
      item.make should equal("BMW")
      item.model should equal("Mini")

      // map function
      val redCarsPrice =
        """(doc: dispatch.json.JsValue) => {
          val car = sjson.json.JsBean.fromJSON(doc, Some(classOf[scouch.db.CarSaleItem]));
          if (car.color.contains("Red")) List(List(car.make, car.price)) else Nil
        }"""

      // map function
      val redCars =
        """(doc: dispatch.json.JsValue) => {
          val car = sjson.json.JsBean.fromJSON(doc, Some(classOf[scouch.db.CarSaleItem]));
          if (car.color.contains("Red")) List(List(car.make, car)) else Nil
        }"""

      val redCarsView = new View(redCars, null)
      val redCarsPriceView = new View(redCarsPrice, null)

      val cv = DesignDocument("car_views", null, Map[String, View](), null)
      cv.language = "scala"

      val rcv = DesignDocument(cv._id, null, Map("red_cars" -> redCarsView, "red_cars_price" -> redCarsPriceView), null)
      rcv.language = "scala"
      http(Doc(carDb, rcv._id) add rcv)

      val ls1 = http(carDb view(
        Views builder("car_views/red_cars") build))
      println(ls1)
      ls1.size should equal(3)
                            
      val ls2 = http(carDb view(
        Views builder("car_views/red_cars_price") build))
      println(ls2)
      ls2.size should equal(3)
    }
  }
  
  describe("View results fetched as JSON and converted to objects after return") {
    it ("should create 3 objects from 3 items in map return") {
      val ls1 = http(carDb view(
        Views builder("car_views/red_cars") build))
      println(ls1)
      ls1.size should equal(3)
      
      import dispatch.json.Js._;
      val objs =
        ls1.map { car =>
          val x = Symbol("value") ? obj
          val x(x_) = car
          JsBean.fromJSON(x_, Some(classOf[CarSaleItem]))
        }
      objs.size should equal(3)
      objs.map(_.make).sortWith((e1, e2) => (e1 compareTo e2) < 0) should equal(List("BMW", "Geo", "Honda"))
    }
  }
  
  describe("Returning objects directly from view results (map only)") {
    it ("should create 3 car objects from 3 items in map return") {
      val ls1 = http(carDb view(
        Views builder("car_views/red_cars") build, classOf[CarSaleItem]))
      println(ls1)
      ls1.size should equal(3)
      ls1.map(_.make).sortWith((e1, e2) => (e1 compareTo e2) < 0) should equal(List("BMW", "Geo", "Honda"))
    }
    it ("should create 3 number objects from 3 items in map return") {
      val ls1 = http(carDb view(
        Views builder("car_views/red_cars_price") build, classOf[BigDecimal]))
      println(ls1)
      ls1.size should equal(3)
      // ls1.sort((e1, e2) => (e1 compareTo e2) < 0) should equal(List(7500, 12500, 16000))
      ls1.sortWith((e1, e2) => (e1 < e2)) should equal(List(7500, 12500, 16000))
    }
  }
  
  describe("Create, query, view : map and reduce") {
    it("should identify red cars") {
       
      // map function
      val redCarsPrice =
        """(doc: dispatch.json.JsValue) => {
          val car = sjson.json.JsBean.fromJSON(doc, Some(classOf[scouch.db.CarSaleItem]));
          if (car.color.contains("Red")) List(List(car.make, car.price)) else Nil
        }"""

      val redfn1 = """(key: List[(String, String)], values: List[dispatch.json.JsNumber], rereduce: Boolean) => {
          values.foldLeft(BigDecimal(0.00))((s, f) => s + (f match { case dispatch.json.JsNumber(n) => n }))
        }"""
      
      val redCarsPriceView = new View(redCarsPrice, redfn1)

      val cv = DesignDocument("red_car_views", null, Map[String, View](), null)
      cv.language = "scala"

      val rcv = DesignDocument(cv._id, null, Map("red_cars_price" -> redCarsPriceView), null)
      rcv.language = "scala"
      http(Doc(carDb, rcv._id) add rcv)

      val ls2 = http(carDb view(
        Views builder("red_car_views/red_cars_price") build))
      println(ls2)
       ls2.size should equal(1)
    }
  }
  
  describe("Create, query, view : map and reduce objects created after returning JSON from view") {
    it("should identify red cars and find sum of price") {
       
      // map function
      val mapForRedCars =
        """(doc: dispatch.json.JsValue) => {
          val car = sjson.json.JsBean.fromJSON(doc, Some(classOf[scouch.db.CarSaleItem]));
          if (car.color.contains("Red")) List(List(car.make, car)) else Nil
        }"""

      val reduceToPriceSum = """(key: List[(String, String)], values: List[dispatch.json.JsObject], rereduce: Boolean) => {
          val objs = values.map(sjson.json.JsBean.fromJSON(_, Some(classOf[scouch.db.CarSaleItem]))).map(_.price)
          objs.foldLeft(BigDecimal(0.00))(_+_)
        }"""
      
      val redCarsPriceView = new View(mapForRedCars, reduceToPriceSum)

      val cv = DesignDocument("sum_car_views", null, Map[String, View](), null)
      cv.language = "scala"

      val rcv = DesignDocument(cv._id, null, Map("red_cars_sum_price" -> redCarsPriceView), null)
      rcv.language = "scala"
      http(Doc(carDb, rcv._id) add rcv)

      val ls1 = http(carDb view(
        Views builder("sum_car_views/red_cars_sum_price") build))
      println(ls1)
      ls1.size should equal(1)
      
      import dispatch.json.Js._
      import dispatch.json._
      val objs =
        ls1.map { mp =>
          val x = Symbol("value") ? num
          val x(x_) = mp
          x_
        }
      objs.size should equal(1)
      objs.head should equal(36000)
    }
    it("should identify red cars and find max price") {
       
      // map function to spit out red cars
      val mapForRedCars =
        """(doc: dispatch.json.JsValue) => {
          val car = sjson.json.JsBean.fromJSON(doc, Some(classOf[scouch.db.CarSaleItem]));
          if (car.color.contains("Red")) List(List(car.make, car)) else Nil
        }"""

      // reduce function to get max price
      val reduceToMaxPrice = """(key: List[(String, String)], values: List[dispatch.json.JsObject], rereduce: Boolean) => {
          values.map(sjson.json.JsBean.fromJSON(_, Some(classOf[scouch.db.CarSaleItem]))).map(_.price).sort((e1,e2) => (e1 > e2)).head
        }"""
      
      val redCarsPriceView = new View(mapForRedCars, reduceToMaxPrice)

      val cv = DesignDocument("max_car_views", null, Map[String, View](), null)
      cv.language = "scala"

      val rcv = DesignDocument(cv._id, null, Map("red_cars_max_price" -> redCarsPriceView), null)
      rcv.language = "scala"
      http(Doc(carDb, rcv._id) add rcv)

      val ls1 = http(carDb view(
        Views builder("max_car_views/red_cars_max_price") build))
      println(ls1)
      ls1.size should equal(1)
      
      import dispatch.json.Js._
      import dispatch.json._
      val objs =
        ls1.map { mp =>
          val x = Symbol("value") ? num
          val x(x_) = mp
          x_
        }
      objs.size should equal(1)
      objs.head should equal(16000)
    }
  }
  
  describe("Returning objects directly from view results in reduce") {
    it ("should create 1 number objects from reduce") {
      val ls1 = http(carDb view(
        Views builder("max_car_views/red_cars_max_price") build, classOf[BigDecimal]))
      println(ls1)
      ls1.size should equal(1)
      ls1(0) should equal(16000)
    }
  }
}

import scala.reflect._

@BeanInfo
case class CarSaleItem(make : String, model : String, price : BigDecimal, condition : String, color : String) {

    def this(make : String, model : String, price : Int, condition : String, color : String) =
        this(make, model, BigDecimal.int2bigDecimal(price), condition, color)

    private [db] def this() = this(null, null, 0, null, null)

    override def toString = "A " + condition + " " + color + " " + make + " " + model + " for $" + price
}

@BeanInfo
case class ItemPrice(store: String, item: String, price: Number) {

    private [db] def this() = this(null, null, null)

    override def toString = "shop = " + store + " for item " + item + " @ " + price
}
