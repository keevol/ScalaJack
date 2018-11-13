package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.lub
import scala.collection.immutable

trait ClassWriter[C] extends IRWriter[C] {
  val context: Context
  val typeTransceiver: IRTransceiver[Type]
  val typeMembers: List[CaseClassTypeAdapter.TypeMember[C]] // <- NOTE: Not a ClassLikeTypeAdapter here!
  val fieldMembers: List[ClassLikeTypeAdapter.FieldMember[C]]
  val isSJCapture: Boolean
  val tt: TypeTag[C]

  private val tpe: Type = tt.tpe
  private val TypeType: Type = typeOf[Type]

  // Hook for subclasses (e.g. Mongo) do to anything needed to handle the db key field(s) as given by the @DBKey annotation
  protected def handleDBKeys[IR](ir: IR, members: List[ClassLikeTypeAdapter.FieldMember[C]])(implicit ops: OpsBase[IR]): IR = ir

  override def write[IR](tagged: TypeTagged[C])(implicit ops: OpsBase[IR], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(null) =>
        WriteSuccess(IRNull())

      case TypeTagged(value) =>
        val errorsBuilder = immutable.Seq.newBuilder[WriteError]

        val fields = scala.collection.mutable.ListBuffer.empty[(String, IR)]

        if (typeMembers.nonEmpty) {
          import scala.collection.mutable

          val setsOfTypeArgsByTypeParam = new mutable.HashMap[Symbol, mutable.HashSet[Type]]

          for (fieldMember <- fieldMembers) {
            val TypeTagged(fieldValue) = fieldMember.valueIn(TypeTagged.inferFromRuntimeClass[C](value))
            val declaredFieldValueType = fieldMember.declaredValueType
            val actualFieldValueType = Reflection.inferTypeOf(fieldValue)(fieldMember.valueTypeTag)

            for (typeParam <- tpe.typeConstructor.typeParams) {
              //
              // TODO: WARNING--typeMember isn't used.  Why are we looping in an inner loop if the value isn't used???
              //
              for (typeMember <- typeMembers) {
                val optionalTypeArg = Reflection.solveForNeedleAfterSubstitution(
                  haystackBeforeSubstitution = declaredFieldValueType,
                  haystackAfterSubstitution  = actualFieldValueType,
                  needleBeforeSubstitution   = typeParam.asType.toType)

                for (typeArg <- optionalTypeArg) {
                  setsOfTypeArgsByTypeParam.getOrElseUpdate(typeParam, new mutable.HashSet[Type]) += typeArg
                }
              }
            }
          }

          val substitutions: List[(Symbol, Type)] = (for ((typeParam, setOfTypes) <- setsOfTypeArgsByTypeParam) yield {
            typeParam -> lub(setOfTypes.toList)
          }).toList

          val substitutionMap = substitutions.toMap

          val typeParams = tpe.typeConstructor.typeParams
          val typeArgs = typeParams.map(typeParam => substitutionMap(typeParam))

          for (typeMember <- typeMembers) {
            val ttt = typeMember.typeSignature.substituteTypes(substitutions.map(_._1), substitutions.map(_._2))
            val typeSerializationResult = typeTransceiver.write(TypeTagged(ttt, TypeType))
            typeSerializationResult match {
              case WriteSuccess(typeIR) =>
                fields += ((typeMember.name.toString, typeIR))
              case WriteFailure(typeErrors) =>
                errorsBuilder ++= typeErrors
            }
          }

          val newType = appliedType(tpe.typeConstructor, typeArgs)
          val newTypeAdapter = context.typeAdapter(newType).as[ClassLikeTypeAdapter[C]]

          for (member <- newTypeAdapter.fieldMembers) {
            val valueSerializationResult = member.writeValue(member.valueIn(tagged))
            valueSerializationResult match {
              case WriteSuccess(valueIR) =>
                fields += ((member.name, valueIR))
              case WriteFailure(valueErrors) =>
                errorsBuilder ++= valueErrors
            }
          }
        } else {
          for (member <- fieldMembers) {
            val taggedMemberValue = member.valueIn(tagged)
            // FIXME              val memberName = mappedFieldsByName.get(member.name).map(_.fieldMapName.get).getOrElse(member.name)
            val memberName = member.name
            val valueSerializationResult = member.writeValue(taggedMemberValue)
            valueSerializationResult match {
              case WriteSuccess(memberValueIR) =>
                fields += ((memberName, memberValueIR))
              case failure @ WriteFailure(_) if failure.isNothing =>
              // Nothing to do here
              case WriteFailure(valueErrors) =>
                errorsBuilder ++= valueErrors
            }
          }
        }

        value match {
          case sjc: SJCapture =>
            sjc.captured.map { cap =>
              val capturedFields = cap.ops.unapplyObject(cap.capturedFields.asInstanceOf[cap.IRType]).get
              capturedFields.foreach {
                case (memberName, memberValue) =>
                  fields += ((memberName, cap.ops.become[IR](memberValue)(ops)))
              }
            }
          case _ =>
        }

        val errors = errorsBuilder.result()
        if (errors.nonEmpty) {
          WriteFailure(errors)
        } else {
          WriteSuccess(handleDBKeys(IRObject(fields), fieldMembers))
        }
    }

}
