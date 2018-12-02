package co.blocke.series5
package typeadapter
package javatime

import java.time.format.DateTimeParseException
import java.time.Period
import scala.util.{ Try, Success, Failure }

object PeriodTypeAdapter extends SimpleTypeAdapter[Period] with StringKind {

  override def read(reader: Reader): Period =
    reader.peek match {
      case TokenType.String =>
        Try(Period.parse(reader.readString())) match {
          case Success(u) => u
          case Failure(u) => throw new DateTimeParseException(u.getMessage + "\n" + reader.showError(), u.asInstanceOf[DateTimeParseException].getParsedString, u.asInstanceOf[DateTimeParseException].getErrorIndex)
        }

      case TokenType.Null =>
        reader.readNull()

      case actual => {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading Period value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: Period, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}
