package co.blocke.scalajack
package typeadapter

object FloatTypeAdapter extends TypeAdapter.=:=[Float] {

  override def read(reader: Reader): Float =
    reader.readFloat()

  override def write(value: Float, writer: Writer): Unit =
    writer.writeFloat(value)

}
