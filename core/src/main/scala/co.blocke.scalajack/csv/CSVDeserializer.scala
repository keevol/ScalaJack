package co.blocke.scalajack
package csv

trait CSVDeserializer[IR] extends WireDeserializer[IR, String] {

  this: Ops[IR, String] =>

  override def deserialize(wire: String): DeserializationResult[IR] =
    if (wire == "")
      DeserializationSuccess(IRNull())
    else {
      val tokens = scala.collection.mutable.ListBuffer.empty[IR]
      var inBlock: Boolean = false
      var quoteTrigger: Boolean = false
      val sb = new scala.collection.mutable.StringBuilder()
      val carray = wire.toCharArray :+ '|'
      carray.zipWithIndex.foreach {
        case (c, i) => c match {
          case '"' =>
            if (quoteTrigger) {
              quoteTrigger = false
              sb.append('"')
            } else {
              quoteTrigger = true
              if (carray(i + 1) != '"')
                inBlock = !inBlock
            }
          case ',' if (!inBlock) =>
            quoteTrigger = false
            tokens.append(inferType(sb.toString))
            sb.clear
          case _ =>
            quoteTrigger = false
            sb.append(c)
        }
      }
      tokens.append(inferType(sb.toString.reverse.tail.reverse))
      DeserializationSuccess(IRArray(tokens.toList))
    }

  // Infers Boolean, Double, Long, String, or Null
  private def inferType(s: String): IR =
    s match {
      case "\""             => applyString("")
      case "true" | "false" => applyBoolean(s.toBoolean)
      case ""               => applyNull()
      case IsLong(l)        => applyLong(l)
      case IsDouble(d)      => applyDouble(d)
      case _                => applyString(s)
    }
}

object IsDouble {
  def unapply(s: String): Option[Double] =
    try {
      Some(s.toDouble)
    } catch {
      case _: NumberFormatException => None
    }
}

object IsLong {
  def unapply(s: String): Option[Long] =
    try {
      Some(s.toLong)
    } catch {
      case _: NumberFormatException => None
    }
}