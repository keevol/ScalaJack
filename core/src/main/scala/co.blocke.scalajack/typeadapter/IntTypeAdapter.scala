package co.blocke.scalajack
package typeadapter

object IntTypeAdapter extends SimpleTypeAdapter[Int] {

  override def read(reader: Reader): Int =
    reader.peek match {
      case TokenType.Number =>
        reader.readInt()
      case TokenType.String =>
        reader.readString().toInt
    }

  override def write(value: Int, writer: Writer): Unit =
    writer.writeInt(value)

}
