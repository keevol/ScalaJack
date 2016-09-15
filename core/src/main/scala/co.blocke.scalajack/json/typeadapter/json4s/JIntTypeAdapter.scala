package co.blocke.scalajack.json.typeadapter.json4s

import co.blocke.scalajack.json.typeadapter.SimpleTypeAdapter
import co.blocke.scalajack.json.{ Reader, TokenType, Writer }
import org.json4s.JInt

object JIntTypeAdapter extends SimpleTypeAdapter[JInt] {

  override def read(reader: Reader): JInt =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.Number ⇒
        JInt(reader.readBigInt())
    }

  override def write(value: JInt, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeBigInt(value.num)
    }

}
