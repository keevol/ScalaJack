package co.blocke.scalajack.json.typeadapter.json4s

import co.blocke.scalajack.json.typeadapter.SimpleTypeAdapter
import co.blocke.scalajack.json.{ Reader, TokenType, Writer }
import org.json4s.JDecimal

object JDecimalTypeAdapter extends SimpleTypeAdapter[JDecimal] {

  override def read(reader: Reader): JDecimal =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        JDecimal(reader.readBigDecimal())
    }

  override def write(value: JDecimal, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeBigDecimal(value.num)
    }

}
