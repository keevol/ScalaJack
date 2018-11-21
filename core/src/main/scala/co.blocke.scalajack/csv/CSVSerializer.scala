package co.blocke.scalajack
package csv

import scala.collection.mutable.StringBuilder
import org.apache.commons.text.StringEscapeUtils

trait CSVSerializer[IR] extends WireSerializer[IR, String] {

  this: Ops[IR, String] =>

  override def serialize(ir: IR, sj: ScalaJackLike[_, _]): String = {
    ir match {
      case IRNull() => ""
      case IRArray(elements) =>
        val builder = new StringBuilder()
        elements.zipWithIndex.foreach {
          case (element, index) =>
            if (index > 0) {
              builder.append(",")
            }
            builder.append(fieldRender(element)(this))
        }
        builder.result()
      case IRObject(fields) =>
        serialize(IRArray(fields.map(_._2)), sj)
      case _ =>
        throw new WriteException(WriteFailure(WriteError.ExceptionThrown(new UnsupportedOperationException(s"CSV serialization of input of type ${ir} is unsupported."))))
    }
  }

  private def fieldRender(ir: IR)(implicit ops: Ops[IR, String]): String = {
    var skipEscape = false
    val firstPass = ir match {
      case IRNull() => ""
      case IRString(s) =>
        if (s.isEmpty()) {
          skipEscape = true
          "\"\""
        } else
          s
      case IRLong(l)    => l.toString
      case IRInt(i)     => i.toString
      case IRDouble(d)  => d.toString
      case IRBoolean(b) => b.toString
      case IRDecimal(d) => d.toString
      case x =>
        val e = WriteError.ExceptionThrown(new UnsupportedOperationException(s"CSV serialization of a field of type ${x.getClass.getSimpleName} is unsupported."))
        throw new WriteException(WriteFailure(e))
    }
    if (!skipEscape)
      StringEscapeUtils.escapeCsv(firstPass)
    else
      firstPass
  }
}