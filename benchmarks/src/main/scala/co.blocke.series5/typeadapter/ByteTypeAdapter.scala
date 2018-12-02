package co.blocke.series5
package typeadapter

object ByteTypeAdapter extends SimpleTypeAdapter[Byte] {

  override def read(reader: Reader): Byte =
    reader.readByte()

  override def write(value: Byte, writer: Writer): Unit =
    writer.writeByte(value)

}
