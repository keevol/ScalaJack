package co.blocke.series5
package typeadapter

object LongTypeAdapter extends SimpleTypeAdapter[Long] {

  override def read(reader: Reader): Long =
    reader.readLong()

  override def write(value: Long, writer: Writer): Unit =
    writer.writeLong(value)

}
