package co.blocke.scalajack
package mongo
package typeadapter

import org.mongodb.scala.bson.ObjectId

object BsonObjectIdTypeAdapter extends TypeAdapter.===[ObjectId] {

  val CUSTOM_LABEL = "ObjectId"

  override val irTransceiver: IRTransceiver[ObjectId] = new IRTransceiver[ObjectId] {

    self =>

    override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[ObjectId] =
      ir match {
        case IRNull() => ReadSuccess(TypeTagged(null.asInstanceOf[ObjectId], typeOf[ObjectId]))
        case IRCustom(CUSTOM_LABEL, oidStringIR) =>
          val IRString(hex) = oidStringIR
          ReadSuccess(TypeTagged(new ObjectId(hex), typeOf[ObjectId]))
        case IRString(oidHex) =>
          ReadSuccess(TypeTagged(new ObjectId(oidHex), typeOf[ObjectId]))
        case _ =>
          ReadFailure(path, ReadError.Unexpected("Expected a Bson ObjectId value", reportedBy = self))
      }

    override def write[IR, WIRE](tagged: TypeTagged[ObjectId])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      tagged match {
        case TypeTagged(null)          => WriteSuccess(IRNull())
        case TypeTagged(oid: ObjectId) => WriteSuccess(IRCustom(CUSTOM_LABEL, IRString(oid.toHexString)))
      }
  }
}
