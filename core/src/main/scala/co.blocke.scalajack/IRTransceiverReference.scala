package co.blocke.scalajack

import java.util.concurrent.atomic.AtomicReference

class IRTransceiverReference[T](initialTransceiver: IRTransceiver[T]) extends IRTransceiver[T] {

  private val ref = new AtomicReference[IRTransceiver[T]]

  override def toString: String = s"IRTransceiverReference($referencedTransceiver)"

  def referencedTransceiver: IRTransceiver[T] = ref.get()

  def referencedTransceiver_=(irTransceiver: IRTransceiver[T]): Unit = {
    require(irTransceiver ne null, "Referenced transceiver must not be null")
    ref.set(irTransceiver)
  }

  referencedTransceiver = initialTransceiver

  override def read[IR](path: Path, ir: IR)(implicit ops: OpsBase[IR], guidance: SerializationGuidance): ReadResult[T] =
    ref.get().read[IR](path, ir)

  override def write[IR](tagged: TypeTagged[T])(implicit ops: OpsBase[IR], guidance: SerializationGuidance): WriteResult[IR] =
    ref.get().write[IR](tagged)
}
