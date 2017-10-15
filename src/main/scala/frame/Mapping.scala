package frame

import scala.collection.mutable

class Mapping {
  var whoop = 0L
  private val raw = new mutable.HashMap[String, Long]

  def contains(key: String) = raw.get(key)

  def getOrUpdate(key: String) = {
    val id = raw.getOrElseUpdate(key, whoop)
    whoop = whoop +1
    id
  }

}
