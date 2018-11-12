package co.blocke.scalajack
package csv

trait CSVParser extends Parser[String] {

  def _parse[AST](source: String)(implicit ops: AstOps[AST, String]): Option[AST] = {
    if (source == "")
      Some(AstNull())
    else {
      val tokens = scala.collection.mutable.ListBuffer.empty[AST]
      var inBlock: Boolean = false
      var quoteTrigger: Boolean = false
      val sb = new scala.collection.mutable.StringBuilder()
      val carray = source.toCharArray :+ '|'
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
      Some(ops.applyArray(tokens.toList))
    }
  }

  // Infers Boolean, Double, Long, String, or Null
  private def inferType[AST](s: String)(implicit ops: AstOps[AST, String]): AST = {
    //    val trimmed = s.trim
    s match {
      case "\""             => ops.applyString("")
      case "true" | "false" => ops.applyBoolean(s.toBoolean)
      case ""               => ops.applyNull()
      case IsLong(l)        => ops.applyLong(l)
      case IsDouble(d)      => ops.applyDouble(d)
      case _                => ops.applyString(s)
    }
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