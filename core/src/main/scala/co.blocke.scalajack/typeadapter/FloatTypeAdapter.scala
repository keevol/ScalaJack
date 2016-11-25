package co.blocke.scalajack
package typeadapter

object FloatTypeAdapter extends SimpleTypeAdapter[Float] {

  override def read(reader: Reader): Float =
    reader.peek match {
      case TokenType.Number =>
        reader.readFloat()
      case TokenType.String =>
        reader.readString().toFloat
    }

  override def write(value: Float, writer: Writer): Unit =
    writer.writeFloat(value)

}
