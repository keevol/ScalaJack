package co.blocke.scalajack
package typeadapter

import org.apache.commons.codec.binary.Hex
import scala.collection.JavaConverters._

object BinaryTypeAdapter extends TypeAdapter.=:=[Array[Byte]] {

  val CUSTOM_LABEL = "Binary"

  override val irTransceiver: IRTransceiver[Array[Byte]] = new IRTransceiver[Array[Byte]] {

    self =>

    private val ArrayOfByteType: Type = typeOf[Array[Byte]]
    private val taggedNull: TypeTagged[Array[Byte]] = TypeTagged[Array[Byte]](null, ArrayOfByteType)

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Array[Byte]] =
      ir match {
        case IRNull()    => ReadSuccess(taggedNull)
        case IRString(s) => ReadSuccess(TypeTagged(Hex.decodeHex(s), ArrayOfByteType))
        case _ =>
          ReadFailure(path, ReadError.Unexpected("Expected hex string (binary data)", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[Array[Byte]])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      WriteSuccess(IRString(Hex.encodeHexString(tagged.get)))
  }
}
