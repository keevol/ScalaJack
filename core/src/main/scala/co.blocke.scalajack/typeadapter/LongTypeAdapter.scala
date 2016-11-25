package co.blocke.scalajack
package typeadapter

object LongTypeAdapter extends SimpleTypeAdapter[Long] {

  override def read(reader: Reader): Long =
    reader.peek match {
      case TokenType.Number =>
        reader.readLong()
      case TokenType.String =>
        reader.readString().toLong
    }

  override def write(value: Long, writer: Writer): Unit =
    writer.writeLong(value)

}
