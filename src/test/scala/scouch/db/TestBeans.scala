package scouch.db

import scala.reflect._
import scala.annotation.target._

import sjson.json._

object TestBeans {
  @BeanInfo
  case class Shop(store: String, item: String, price: Number) {
  
    private def this() = this(null, null, null)
  
    override def toString = "shop = " + store + " for item " + item + " @ " + price
  }
  
  @BeanInfo
  case class Contact(name: String, 
                     @(JSONTypeHint @field)(value = classOf[Address])
                     addresses: Map[String, Address]) {
  
    private def this() = this(null, null)
  
    override def toString = "name = " + name + " addresses = " + addresses.map(a => a._1 + ":" + a._2.toString).mkString(",")
  }

  @BeanInfo
  case class Address(street: String, city: String, zip: String) {
    private def this() = this(null, null, null)
  
    override def toString = "address = " + street + "/" + city + "/" + zip
  }

  @BeanInfo
  case class InternationalAddress(st: String, ct: String, zp: String, cnt: String)
    extends Address(st, ct, zp) {

    override val street = st
    override val city = ct
    override val zip = zp
    val country = cnt

    private def this() = this(null, null, null, null)
  
    override def toString = super.toString + "/" + country
  }
  
  @BeanInfo
  case class AddressWithOptionalCity(street: String, city: Option[String], zip: String) {
  
    private def this() = this(null, None, null)
  
    override def toString = "address = " + street + "/" + 
      (city match {
        case None => ""
        case Some(c) => c
      }) + "/" + zip
  }


  @BeanInfo
  case class ContactWithOptionalAddr(name: String, 
                                @(JSONTypeHint @field)(value = classOf[Address])
                                @(OptionTypeHint @field)(value = classOf[Map[_,_]])
                                addresses: Option[Map[String, Address]]) {
  
    private def this() = this(null, None)
  
    override def toString = "name = " + name + " " + 
      (addresses match {
        case None => ""
        case Some(ad) => " addresses = " + ad.map(a => a._1 + ":" + a._2.toString).mkString(",")
      })
  }

  
  @BeanInfo  
  case class Person(lastName: String, 
               firstName: String,
               @(JSONTypeHint @field)(value = classOf[Address])
               addresses: List[Address]) {
  
    def this() = this(null, null, Nil)
  
    override def toString = "person = " + lastName + "/" + firstName + "/" + addresses
  }

  @BeanInfo
  case class Book(id: Number, 
             title: String, @(JSONProperty @getter)(value = "ISBN") isbn: String) {
  
    def this() = this(0, null, null)
    override def toString = "id = " + id + " title = " + title + " isbn = " + isbn
  }

  @BeanInfo
  case class Author(lastName: String, firstName: String) {
  
    private def this() = this(null, null)
  }

  @BeanInfo
  case class Book_1(title: String, author: Author) {
  
    private def this() = this(null, null)
  }

  @BeanInfo
  case class Journal(id: BigDecimal, 
                     title: String, 
                     author: String, 
                     @(JSONProperty @getter)(ignore = true) issn: String) {

    private def this() = this(0, null, null, null)
    override def toString =
      "Journal: " + id + "/" + title + "/" + author + 
        (issn match {
            case null => ""
            case _ => "/" + issn
          })
  }

  @BeanInfo
  case class Journal_1(id: Int, 
                  title: String, 
                  author: String, 
                  @(JSONProperty @getter)(ignoreIfNull = true)
                  issn: String) {
  }

  @BeanInfo
  class Journal_2(i: Int, t: String, au: String, is: String) {
    val id = i
    val title = t
    val author = au
  
    @(JSONProperty @getter)(value = "ISSN", ignoreIfNull = true)
    val issn = is
  }

  @BeanInfo
  class Item_1(i: String, ps: Map[String, Number]) {
    val item = i
    val prices = ps
  
    def this() = this(null, null)
  }

  @BeanInfo
  class Item_2(i: String, ps: List[Number]) {
    val item = i
    val prices = ps
  
    def this() = this(null, null)
  }
  
  @BeanInfo
  case class Instrument(
    val id: Number, 
    val name: String, 
    @(JSONProperty @getter)(value = "TYPE", ignoreIfNull = false, ignore = false)
    val typ: String) {
    
    private def this() = this(null, null, null)
    override def toString = "id: " + id + " name: " + name + " type: " + typ
  }
  
  @BeanInfo
  case class Trade(
    val ref: String,
    @(JSONProperty @getter)(value = "Instrument", ignoreIfNull = false, ignore = false)
    val ins: Instrument,
    val amount: Number) {
      
    private def this() = this(null, null, null)
    override def toString = "ref: " + ref + " ins: " + ins + " amount: " + amount
  }
  
  @BeanInfo
  case class Salary(val basic: Number, val allowance: Number) {
    private def this() = this(null, null)
  }
  
  @BeanInfo
  class Employee(
    val id: Number,
    val name: String,
    
    @(JSONProperty @getter)(value = "Previous Employer", ignoreIfNull = true, ignore = false)
    val prevEmployer: String,
    
    @(JSONProperty @getter)(value = "Addresses")
    @(JSONTypeHint @field)(value = classOf[Address])
    val addresses: List[Address],
    
    @JSONProperty("Salary")
    @(JSONProperty @getter)(value = "Salary")
    val sal: Salary
  ) {
    private def this() = this(null, null, null, Nil, null)
  }

  @BeanInfo
  case class Foo(str: String, valid: Boolean) {
    private def this() = this(null, false)
  }

  @BeanInfo
  case class Bar(str: String, int: Int, lng: Long, flt: Float, valid: Boolean) {
    private def this() = this(null, 0, 0l, 0f, false)
  }

  import java.util.Date
  @BeanInfo
  case class SecurityTrade(no: String, tdate: Date, sdate: Date, amount: BigDecimal) {
    private def this() = this(null, null, null, null)
  }
}

