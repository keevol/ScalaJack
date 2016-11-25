package co.blocke.scalajack
package typeadapter

object ShortTypeAdapter extends SimpleTypeAdapter[Short] {

  override def read(reader: Reader): Short =
    reader.peek match {
      case TokenType.Number =>
        reader.readShort()
      case TokenType.String =>
        reader.readString().toShort
    }

  override def write(value: Short, writer: Writer): Unit =
    writer.writeShort(value)

}
