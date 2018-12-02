package co.blocke.series5
package typeadapter

object CharTypeAdapter extends SimpleTypeAdapter[Char] with StringKind {

  override def read(reader: Reader): Char = {
    reader.readString().head // TODO Ensure there is only one character
  }

  override def write(value: Char, writer: Writer): Unit =
    writer.writeChar(value)

}
