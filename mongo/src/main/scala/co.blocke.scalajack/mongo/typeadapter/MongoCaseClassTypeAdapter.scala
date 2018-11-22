package co.blocke.scalajack
package mongo
package typeadapter

import co.blocke.scalajack.typeadapter.{ CaseClassTypeAdapter, ClassWriter, ClassLikeTypeAdapter, ClassReaderUsingReflectedConstructor }

object MongoCaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  val IdMemberName: MemberName = "_id"

  // Some magic because Tuples are also case classes, but they have their own type adapter, and we *don't*
  // want the case class machinery to kick in for Tuples.
  val tupleFullName = """scala.Tuple(\d+)""".r

  /*
  For Mongo case classes, re-use all the core case class machinery except we need to force Mongo-specific serializer/deserializer to handle
  the DBKey representation.  This approach hijack's the normal CaseClassTypeAdapter (which is a factory) and takes the generated
  CaseClassTypeAdapter (instance) and re-wires it to Mongo-specific ser/deser.
   */
  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isCaseClass) {
      classSymbol.fullName match {
        case tupleFullName(_) =>
          next.typeAdapterOf[T]
        case _ =>
          val ccta = next.typeAdapterOf[T].as[CaseClassTypeAdapter[T]]
          val constructorMirror = ccta.irTransceiver.asInstanceOf[ClassReaderUsingReflectedConstructor[T]].constructorMirror

          ccta.copy(
            irTransceiver = new MongoCaseClassIRTransceiver(
              ccta.dbKeys,
              IdMemberName,
              context,
              constructorMirror,
              context.typeAdapterOf[Type].irTransceiver,
              ccta.typeMembers.asInstanceOf[List[CaseClassTypeAdapter.TypeMember[T]]],
              ccta.fieldMembers,
              false,
              tt))
      }
    } else
      next.typeAdapterOf[T]
}

class MongoCaseClassIRTransceiver[C](
    dbKeys:                List[ClassLikeTypeAdapter.FieldMember[_]],
    idMemberName:          String,
    val context:           Context,
    val constructorMirror: MethodMirror,
    val typeTransceiver:   IRTransceiver[Type],
    val typeMembers:       List[CaseClassTypeAdapter.TypeMember[C]],
    val fieldMembers:      List[ClassLikeTypeAdapter.FieldMember[C]],
    val isSJCapture:       Boolean,
    val tt:                TypeTag[C]) extends IRTransceiver[C] with ClassReaderUsingReflectedConstructor[C] with ClassWriter[C] {

  // Reading
  override def handleDBKeys[IR](path: Path, ir: IR, members: List[ClassLikeTypeAdapter.FieldMember[C]])(implicit ops: OpsBase[IR]): Either[ReadFailure, IR] =
    dbKeys.size match {
      case 0 => Right(ir)
      case 1 =>
        val keyFieldName = dbKeys.head.name
        var keyFound = false
        val resultIR = ops.mapObjectFields(ir, { (fieldname, value) =>
          fieldname match {
            case s: String if s == idMemberName =>
              keyFound = true
              (keyFieldName, value)
            case _ => (fieldname, value)
          }
        })
        if (keyFound)
          Right(IRObject(resultIR))
        else
          Left(ReadFailure(path, ReadError.Unexpected(s"Did not find required db key field (e.g. _id)", this)))
      case _ =>
        val fields = scala.collection.mutable.ListBuffer.empty[(String, IR)]
        val keysFound = scala.collection.mutable.ListBuffer.empty[String]
        val IRObject(objFields) = ir
        objFields.foreach {
          case (fieldname, element) =>
            if (fieldname == idMemberName) { // expand id field if found
              val IRObject(idFields) = element
              idFields.foreach { case (fieldname, element) => keysFound.append(fieldname); fields.append((fieldname, element)) }
            } else
              fields.append((fieldname, element))
        }
        if (keysFound.size == dbKeys.size)
          Right(IRObject(fields.toList))
        else {
          val missing = dbKeys.map(_.name).diff(keysFound).mkString("[", ",", "]")
          Left(ReadFailure(path, ReadError.Unexpected(s"Missing at least one required db key field (e.g. _id) component: $missing", this)))
        }
    }

  // Writing
  override protected def handleDBKeys[IR](ir: IR, members: List[ClassLikeTypeAdapter.FieldMember[C]])(implicit ops: OpsBase[IR]): IR =
    dbKeys.size match {
      case 0 => ir // no db keys specified... do nothing and return original ast
      case 1 => // simplified _id : value notation
        val keyFieldName = dbKeys.head.name
        IRObject(ops.mapObjectFields(ir, { (fieldname, value) =>
          fieldname match {
            case s: String if s == keyFieldName => (idMemberName, value)
            case _                              => (fieldname, value)
          }
        }))
      case _ => // compound notation _id : { key:value, key:value}
        val dbkeyFieldNames = dbKeys.map(_.name)
        val (irDBfields, IRObject(irNonDBfields)) = ops.partitionObject(ir, (name, _) => dbkeyFieldNames.contains(name))
        //        val irNonDBfields = IRObject.unapply(nonDBObj)
        val merged = (idMemberName, irDBfields.asInstanceOf[IR]) +: irNonDBfields.asInstanceOf[Seq[(String, IR)]]
        IRObject(merged)
    }
}
