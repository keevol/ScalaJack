package co.blocke.scalajack
package typeadapter

import scala.collection.{ immutable, mutable }
import scala.util.Try
import scala.util.control.NonFatal

trait ClassReaderUsingReflectedConstructor[CC] extends IRReader[CC] {

  self: IRTransceiver[CC] =>

  val context: Context
  val constructorMirror: MethodMirror
  val typeTransceiver: IRTransceiver[Type]
  val typeMembers: List[CaseClassTypeAdapter.TypeMember[CC]]
  val fieldMembers: List[ClassLikeTypeAdapter.FieldMember[CC]]
  val isSJCapture: Boolean
  val tt: TypeTag[CC]

  private type TypeMember = CaseClassTypeAdapter.TypeMember[CC]
  private type FieldMember = ClassLikeTypeAdapter.FieldMember[CC]

  private val caseClassType: Type = tt.tpe
  private val nullTypeTagged: TypeTagged[CC] = TypeTagged[CC](null.asInstanceOf[CC], caseClassType)
  private val typeMembersByName: Map[String, TypeMember] = typeMembers.map(typeMember => typeMember.name -> typeMember).toMap
  private val fieldMembersByName: Map[String, FieldMember] = fieldMembers.map(fieldMember => fieldMember.name -> fieldMember).toMap

  // Hook for subclasses (e.g. Mongo) do to anything needed to handle the db key field(s) as given by the @DBKey annotation
  protected def handleDBKeys[IR](path: Path, ir: IR, members: List[ClassLikeTypeAdapter.FieldMember[CC]])(implicit ops: OpsBase[IR]): Either[ReadFailure, IR] = Right(ir)

  private def inferConcreteReader[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): Option[IRReader[_ <: CC]] =
    if (typeMembers.isEmpty) {
      None
    } else {
      ir match {
        case IRObject(fields) =>

          import scala.collection.mutable

          val setsOfTypeArgsByTypeParam = new mutable.HashMap[Symbol, mutable.HashSet[Type]]

          fields.foreach {
            case (fieldName, fieldValueAst) =>
              for (typeMember <- typeMembersByName.get(fieldName)) {
                val actualType: Type = Try(typeTransceiver.read(path \ fieldName, fieldValueAst)).map(_.get).getOrElse(typeMember.baseType)

                // Solve for each type parameter
                for (typeParam <- caseClassType.typeConstructor.typeParams) {
                  val optionalTypeArg = Reflection.solveForNeedleAfterSubstitution(
                    haystackBeforeSubstitution = typeMember.typeSignature,
                    haystackAfterSubstitution  = actualType,
                    needleBeforeSubstitution   = typeParam.asType.toType)

                  for (typeArg <- optionalTypeArg) {
                    setsOfTypeArgsByTypeParam.getOrElseUpdate(typeParam, new mutable.HashSet[Type]) += typeArg
                  }
                }
              }
          }

          val typeArgs = for (typeParam <- caseClassType.typeConstructor.typeParams) yield {
            val possibleTypeArgs = setsOfTypeArgsByTypeParam(typeParam).toList
            val typeArg :: Nil = possibleTypeArgs
            typeArg
          }

          val concreteType = appliedType(caseClassType.typeConstructor, typeArgs)

          if (concreteType =:= caseClassType) {
            // YAY! BUSINESS AS USUAL
            None
          } else {
            Some(context.irTransceiver(concreteType).asInstanceOf[IRTransceiver[CC]])
          }

        case _ =>
          None
      }
    }

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[CC] =
    ir match {
      case IRNull() =>
        ReadSuccess(nullTypeTagged)

      case IRObject(_) =>
        try {
          handleDBKeys(path, ir, fieldMembers) match {
            case Left(fail) => fail
            case Right(postDBKey) =>
              val readResultsByField: mutable.Map[FieldMember, ReadResult[Any]] = new mutable.HashMap[FieldMember, ReadResult[Any]]

              inferConcreteReader(path, postDBKey) match {
                case Some(concreteReader) =>
                  concreteReader.read(path, ir)

                case None =>
                  val irobj @ IRObject(fields) = postDBKey
                  fields.foreach {
                    case (fieldName, fieldValueIR) =>
                      for (fieldMember <- fieldMembersByName.get(fieldName)) {
                        readResultsByField(fieldMember) = fieldMember.readValue[IR, WIRE](path \ fieldName, fieldValueIR)
                      }
                  }

                  //                            println(fieldMembers)
                  //                            println(fieldMembersByName)

                  // Missing fields in JSON... let's go deeper...
                  for (fieldMember <- fieldMembers if !readResultsByField.contains(fieldMember)) {
                    // Substitute any specified default values
                    readResultsByField(fieldMember) = if (fieldMember.defaultValue.isDefined)
                      ReadSuccess(TypeTagged(fieldMember.defaultValue.get, fieldMember.declaredValueType))
                    else
                      fieldMember.readValueFromNothing[IR, WIRE](path \ fieldMember.name) match {
                        case rs @ ReadSuccess(_) => rs
                        case ReadFailure(Seq(pathErr)) if (pathErr._2.isInstanceOf[ReadError.Unsupported]) =>
                          ReadFailure(pathErr._1, ReadError.Missing(fieldMember.name, fieldMember.valueTypeAdapter.resolved.irTransceiver))
                      }
                  }

                  if (readResultsByField.exists(_._2.isFailure)) {
                    // Uh-oh!  One or more fields *still* didn't deserialize (after default value substitution).
                    ReadFailure(readResultsByField.values.flatMap(_.errors).to[immutable.Seq])
                  } else {
                    val constructorArguments: Array[Any] = fieldMembers
                      .map { fieldMember =>
                        val ReadSuccess(TypeTagged(fieldValue)) = readResultsByField(fieldMember)
                        fieldValue
                      }
                      .toArray

                    val instanceOfCaseClass = constructorMirror.apply(constructorArguments: _*).asInstanceOf[CC]

                    if (isSJCapture) {
                      val partitionedFields = ops.partitionObject(irobj, (name, _) => fieldMembersByName.keySet.contains(name))
                      val captured = partitionedFields._2.asInstanceOf[ops.ObjectType] // fields not in class we need to save

                      val aux = ops.asInstanceOf[Ops.Aux[IR, WIRE, ops.ObjectType]]
                      instanceOfCaseClass.asInstanceOf[SJCapture].captured = Some(IRAndOps[IR, WIRE, ops.ObjectType](captured)(aux))
                    }
                    ReadSuccess(TypeTagged[CC](instanceOfCaseClass, caseClassType))
                  }
              }
          }
        } catch {
          case NonFatal(e) =>
            ReadFailure(path, ReadError.ExceptionThrown(e))
        }

      case IRString(s) if (guidance.isMapKey) =>
        try {
          ops.deserialize(s.asInstanceOf[WIRE]).mapToReadResult(path, (dsIR: IR) => this.read(Path.Root, dsIR)(ops, guidance = guidance.copy(isMapKey = false)))
        } catch {
          case t: Throwable => ReadFailure(path, ReadError.ExceptionThrown(t))
        }

      case _ =>
        ReadFailure(path, ReadError.Unexpected(s"Expected a JSON object, not $ir", self))
    }
}
