package co.blocke.scalajack
package typeadapter

object ByteTypeAdapter extends SimpleTypeAdapter[Byte] {

  override def read(reader: Reader): Byte =
    reader.peek match {
      case TokenType.Number =>
        reader.readByte()
      case TokenType.String =>
        reader.readString().toByte
    }

  override def write(value: Byte, writer: Writer): Unit =
    writer.writeByte(value)

}
