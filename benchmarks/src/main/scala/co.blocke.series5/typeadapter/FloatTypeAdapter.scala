package co.blocke.series5
package typeadapter

object FloatTypeAdapter extends SimpleTypeAdapter[Float] {

  override def read(reader: Reader): Float =
    reader.readFloat()

  override def write(value: Float, writer: Writer): Unit =
    writer.writeFloat(value)

}
