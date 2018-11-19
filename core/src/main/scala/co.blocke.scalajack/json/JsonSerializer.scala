package co.blocke.scalajack
package json

import java.time.Instant
import org.apache.commons.text.StringEscapeUtils.escapeJava
import typeadapter.javatime._

trait JsonSerializer[IR] extends WireSerializer[IR, String] {

  this: Ops[IR, String] =>

  def serialize(ir: IR, sj: ScalaJackLike[_, _]): String = {
    val builder = new StringBuilder

    val escape: (String) => String = if (sj.isCanonical) escapeJava else (s: String) => s

    def appendString(builder: StringBuilder, string: String): Unit = {
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
              case _ =>
                builder.append('"')
                i += 1
                builder.appendAll(string.substring(beginIndex))
                builder.append('"')
            }
        }
      }
    }

    def privateSerMapFields(sepChar: String, builder: StringBuilder, fields: Seq[(IR, IR)]) =
      fields.foreach {
        case (IRString(kString), v) => // Special case: Key is already a String, ready-to-eat
          builder.append(sepChar)
          appendString(builder, escape(kString))
          builder.append(":")
          helper(builder, v)
        case (k, v) =>
          builder.append(sepChar)
          val subBuilder = new StringBuilder()
          helper(subBuilder, k)
          val kStr = subBuilder.result()
          if (!kStr.isEmpty && (kStr(0) == '"' || !sj.isCanonical))
            builder.append(kStr)
          else
            appendString(builder, escape(subBuilder.result))
          builder.append(":")
          helper(builder, v)
      }

    def serializeMapFields(builder: StringBuilder, fields: Seq[(IR, IR)]) = {
      builder.append('{')
      if (!fields.isEmpty) {
        val (firstField :: restFields) = fields
        privateSerMapFields("", builder, Seq(firstField))
        privateSerMapFields(",", builder, restFields)
      }
      builder.append('}')
    }

    def serializeCustom(builder: StringBuilder, customLabel: String, customIR: IR) =
      customLabel match {
        case InstantTypeAdapter.CUSTOM_LABEL =>
          val IRArray(epocnano) = customIR
          val Seq(IRLong(epocSec), IRInt(nano)) = epocnano
          appendString(builder, Instant.ofEpochSecond(epocSec, nano.intValue()).toString)
        case _ =>
          helper(builder, customIR)
      }

    def serializeArray(builder: StringBuilder, elements: Seq[IR]) = {
      builder.append('[')
      elements.zipWithIndex.map {
        case (ir, index) =>
          if (index > 0)
            builder.append(",")
          helper(builder, ir)
      }
      builder.append(']')
    }

    def serializeObject(builder: StringBuilder, fields: Seq[(String, IR)]) = {
      builder.append('{')
      var isFirst = true
      fields.map {
        case (name, value) =>
          if (isFirst) {
            isFirst = false
          } else {
            builder.append(",")
          }
          if (sj.isCanonical)
            appendString(builder, escape(name))
          else {
            appendString(builder, name)
          }
          builder.append(":")
          helper(builder, value)
      }
      builder.append('}')
    }

    def helper(builder: StringBuilder, ir: IR): Unit =
      ir match {
        case IRMap(fields)                     => serializeMapFields(builder, fields)
        case IRCustom((customLabel, customIR)) => serializeCustom(builder, customLabel, customIR)
        case IRArray(elements)                 => serializeArray(builder, elements)
        case IRBoolean(booleanValue)           => builder.append(if (booleanValue) "true" else "false")
        case IRDecimal(bigDecimal)             => builder.append(bigDecimal.toString)
        case IRDouble(doubleValue)             => builder.append(doubleValue)
        case IRInt(bigInt)                     => builder.append(bigInt.toString)
        case IRLong(longValue)                 => builder.append(longValue)
        case IRNull()                          => builder.append("null")
        case IRObject(fields)                  => serializeObject(builder, fields)
        case IRString(string)                  => appendString(builder, escape(string))
      }

    helper(builder, ir)

    builder.result()
  }
}
