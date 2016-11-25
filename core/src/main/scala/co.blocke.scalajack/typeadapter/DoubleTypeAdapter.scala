package co.blocke.scalajack
package typeadapter

object DoubleTypeAdapter extends SimpleTypeAdapter[Double] {

  override def read(reader: Reader): Double =
    reader.peek match {
      case TokenType.Number =>
        reader.readDouble()
      case TokenType.String =>
        reader.readString().toDouble
    }

  override def write(value: Double, writer: Writer): Unit =
    writer.writeDouble(value)

}
