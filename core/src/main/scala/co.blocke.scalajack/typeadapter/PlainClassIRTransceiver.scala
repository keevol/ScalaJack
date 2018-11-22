package co.blocke.scalajack
package typeadapter

import scala.collection.{ immutable, mutable }
import co.blocke.scalajack.typeadapter.PlainClassTypeAdapter.PlainFieldMember

class PlainClassIRTransceiver[T](members: List[PlainFieldMember[T]], newInstance: () => T, isSJCapture: Boolean)(implicit tt: TypeTag[T]) extends IRTransceiver[T] {
  self =>

  private type Member = PlainFieldMember[T]
  private val instanceType: Type = tt.tpe
  private val nullTypeTagged: TypeTagged[T] = TypeTagged(null.asInstanceOf[T], instanceType)
  private val membersByName: Map[String, Member] = members.map(member => member.name -> member).toMap

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[T] =
    ir match {
      case IRNull() =>
        ReadSuccess(nullTypeTagged)

      case irobj @ IRObject(fields) =>
        ReadResult(path) {
          val readResultsByMember = new mutable.HashMap[Member, ReadResult[Any]]

          fields.foreach {
            case (name, valueAst) =>
              for (member <- membersByName.get(name)) {
                val readResult = member.irTransceiver.read(path \ name, valueAst)
                readResultsByMember += member -> readResult
              }
          }

          for (member <- members if !readResultsByMember.contains(member)) {
            val result = member.irTransceiver.readFromNothing(path \ member.name) match {
              // Change generic error about "readFromNothing" not being impliemented, to a more meaningful Missing field error
              case rf: ReadFailure if (rf.errors.size > 0 && rf.errors(0)._2.isInstanceOf[ReadError.Unsupported]) => ReadFailure(path, ReadError.Missing(member.name, this))
              case other => other
            }
            readResultsByMember += member -> result
          }

          if (readResultsByMember.values.exists(_.isFailure)) {
            throw new ReadException(ReadFailure(readResultsByMember.values.flatMap(_.errors).to[immutable.Seq]))
          } else {
            val instance = newInstance()

            for ((member, readResult) <- readResultsByMember) {
              val ReadSuccess(TypeTagged(memberValue)) = readResult
              member.valueSet(instance, memberValue.asInstanceOf[member.Value])
            }

            if (isSJCapture) {
              val partitionedFields = ops.partitionObject(irobj, (name, _) => membersByName.keySet.contains(name))
              val captured = partitionedFields._2.asInstanceOf[ops.ObjectType] // fields not in class we need to save

              val aux = ops.asInstanceOf[Ops.Aux[IR, WIRE, ops.ObjectType]]
              instance.asInstanceOf[SJCapture].captured = Some(IRAndOps[IR, WIRE, ops.ObjectType](captured)(aux))
            }
            TypeTagged(instance, instanceType)
          }
        }

      case _ =>
        ReadFailure(path, ReadError.Unexpected("Expected a JSON object", reportedBy = self))
    }

  override def write[IR, WIRE](tagged: TypeTagged[T])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(null) =>
        WriteSuccess(IRNull())

      case TypeTagged(obj) =>
        val fields = mutable.ListBuffer.empty[(String, IR)]

        //        SerializationSuccess(
        //          AstObject { appendField =>
        for (member <- members) {
          val memberName = member.name
          val memberValue = member.valueIn(tagged)

          member.writeValue(memberValue) match {
            case WriteSuccess(memberValueIR)                     => fields += ((memberName, memberValueIR))
            case WriteFailure(f) if f == Seq(WriteError.Nothing) => // do nothing--ignore optional fields of value None
          }
        }

        if (isSJCapture) {
          val sjc = obj.asInstanceOf[SJCapture]
          sjc.captured.map { cap =>
            val capturedFields = cap.ops.unapplyObject(cap.capturedFields.asInstanceOf[cap.IRType]).get
            capturedFields.foreach {
              case (memberName, memberValue) =>
                fields += ((memberName, cap.ops.become[IR](memberValue)(ops)))
            }
          }
        }

        WriteSuccess(IRObject(fields))

    }
}
