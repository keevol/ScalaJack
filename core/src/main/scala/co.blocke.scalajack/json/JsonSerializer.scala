package co.blocke.scalajack
package json

import java.time.Instant
import org.apache.commons.text.StringEscapeUtils.escapeJava
import typeadapter.javatime._

trait JsonSerializer[IR] extends WireSerializer[IR, String] {

  this: Ops[IR, String] =>

  def serialize(ir: IR, sj: ScalaJackLike[_, _]): String = {
    val builder = new StringBuilder

    def appendString(string: String): Unit = {
      var i = 0

      var beginIndex = 0

      if (sj.isCanonical) {
        builder.append('"' + string + '"')
      } else {
        string match {
          case "" => builder.append("\"\"")
          case _ =>
            string.charAt(i) match {
              case '{' | '[' =>
                builder.appendAll(string)
                beginIndex = i
                i += 1

              case No_Quote_Marker => // no-quotes rendering
                builder.appendAll(string.tail)
                beginIndex = i
                i += 1

              case _ =>
                builder.append('"')
                i += 1
                builder.appendAll(string.substring(beginIndex))
                builder.append('"')
            }
        }
      }
    }

    def helper(ast: IR): Unit =
      ast match {
        case IRCustom((customLabel, customIR)) =>
          customLabel match {
            case InstantTypeAdapter.CUSTOM_LABEL =>
              val IRArray(epocnano) = customIR
              val Seq(IRLong(epocSec), IRInt(nano)) = epocnano
              builder.append('"' + Instant.ofEpochSecond(epocSec, nano.intValue()).toString + '"')
            case _ => helper(customIR)
          }

        case IRArray(x) =>
          builder.append('[')
          x.zipWithIndex.map {
            case (ir, index) =>
              if (index > 0)
                builder.append(",")
              else
                helper(ir)
          }
          builder.append(']')

        case IRBoolean(booleanValue) =>
          builder.append(if (booleanValue) "true" else "false")

        case IRDecimal(bigDecimal) =>
          builder.append(bigDecimal.toString)

        case IRDouble(doubleValue) =>
          builder.append(doubleValue)

        case IRInt(bigInt) =>
          builder.append(bigInt.toString)

        case IRLong(longValue) =>
          builder.append(longValue)

        case IRNull() =>
          builder.append("null")

        case IRObject(x) =>
          builder.append('{')
          var isFirst = true
          x.map {
            case (name, value) =>
              if (isFirst) {
                isFirst = false
              } else {
                builder.append(",")
              }
              if (sj.isCanonical)
                appendString(escapeJava(name))
              else {
                appendString(name)
              }
              builder.append(":")
              helper(value)
          }
          builder.append('}')

        case IRString(string) =>
          appendString(escapeJava(string))
      }

    helper(ir)

    builder.result()
  }
}
