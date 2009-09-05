package scouch.db

trait BulkDocAttribute
case class Id_(id: String) extends BulkDocAttribute
case class Rev_(rev: String) extends BulkDocAttribute
case class Deleted_(value: Boolean) extends BulkDocAttribute

object BulkDocument {
  
  class BulkDocumentBuilder[T <: AnyRef](obj: Option[T], options: List[BulkDocAttribute]) {
    def id(id: String) = new BulkDocumentBuilder(obj, Id_(id) :: options)
    def rev(rev: String) = new BulkDocumentBuilder(obj, Rev_(rev) :: options)
    def deleted(del: Boolean) = new BulkDocumentBuilder(None, Deleted_(del) :: options)
    
    def build = BulkDocument(obj, options)
  }
  
  def bulkBuilder[T <: AnyRef](obj: Option[T]) = new BulkDocumentBuilder(obj, List())
}

case class BulkDocument[T <: AnyRef](obj: T, options: List[BulkDocAttribute])
