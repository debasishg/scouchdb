package scouch.db

/**
 * Build the view identified by <tt>nm</tt>, with query options specified by <tt>ops</tt>
 * and keys specified by <tt>ks</tt>.
 * 
 * <p>The list of <tt>keys</tt> specified should be plain Scala objects. The required jsonification
 * will be done as part of implementation. e.g. In order to specify the key set as 
 * <tt>{"keys":[["apple", 0.79],["banana", 3.45]]}</tt>, the <tt>keys</tt> argument should be a Scala 
 * <tt>List</tt> as <pre>List(List("apple", 0.79), List("banana", 3.45))</pre>. Note that all
 * key, startKey, endKey are converted to properly JSON encoded values.
 * 
 * <p>Use the builder pattern through <tt>ViewBuilder</tt> for constructing the view. e.g.
 * <pre>
 *   Views.builder("least_cost/least_cost_lunch")
          .options(optionBuilder descending(true) limit(10) build)
          .keys(List(List("apple", 0.79), List("banana", 0.79)))
          .build
 * </pre>
 * 
 * <p>The method <tt>options</tt> can be built using the DSL builder for <tt>QueryOption</tt>. Refer to
 * @link {QueryOption} for details of the DSL.
 */
private [db] trait Query {
  def name: String
  def options: Option[List[QueryOption[_]]]
  def keys: Option[List[Any]]
  
  def getViewURIFromName = {
    val vs = name split('/')
    if (vs(0).length == 0 || vs(1).length == 0) {
      throw new IllegalArgumentException("Invalid viewname (" + name + ") specified")
    }
    new StringBuilder(name.length)
      .append("_design/")
      .append(vs(0))
      .append("/_view/")
      .append(vs(1)).toString
  }
}
private [db] case class ViewQuery(
  name: String, options: Option[List[QueryOption[_]]], keys: Option[List[Any]]) extends Query 

private [db] case class AdhocViewQuery(
  functions: View, options: Option[List[QueryOption[_]]]) extends Query {
    val name = "_temp_view"
    val keys = None
  }

private [db] case class AllQuery(
  options: Option[List[QueryOption[_]]], keys: Option[List[Any]]) extends Query {
  val name = "_all_docs"
  
  override def getViewURIFromName = name
}


object Views {
  class ViewBuilder(name: String, voptions: Option[List[QueryOption[_]]], vkeys: Option[List[Any]]) {
    
    def options(ops: List[QueryOption[_]]) = 
      new ViewBuilder(name, Some(ops), vkeys)
    
    def keys(ks: List[Any]) =
      new ViewBuilder(name, voptions, Some(ks))
    
    def build = name match {
      case "_all_docs" => new AllQuery(voptions, vkeys)
      case _ => new ViewQuery(name, voptions, vkeys)
    }
  }
  
  class AdhocViewBuilder(fns: View, voptions: Option[List[QueryOption[_]]]) {
    def options(ops: List[QueryOption[_]]) = new AdhocViewBuilder(fns, Some(ops))
    def build = new AdhocViewQuery(fns, voptions)
  }
  
  def builder(name: String) = new ViewBuilder(name, None, None)
  def adhocBuilder(fns: View) = new AdhocViewBuilder(fns, None)
}
