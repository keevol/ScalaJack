package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.lub
import scala.collection.immutable

class ClassSerializer[C](
    context:        Context,
    typeSerializer: Serializer[Type],
    typeMembers:    List[CaseClassTypeAdapter.TypeMember[C]], // <- NOTE: Not a ClassLikeTypeAdapter here!
    fieldMembers:   List[ClassLikeTypeAdapter.FieldMember[C]],
    isSJCapture:    Boolean)(implicit tt: TypeTag[C]) extends Serializer[C] {

  private val tpe: Type = tt.tpe
  private val TypeType: Type = typeOf[Type]

  // Hook for subclasses (e.g. Mongo) do to anything needed to handle the db key field(s) as given by the @DBKey annotation
  protected def handleDBKeys[AST, S](ast: AST, members: List[ClassLikeTypeAdapter.FieldMember[C]])(implicit ops: AstOps[AST, S]): AST = ast

  override def serialize[AST, S](tagged: TypeTagged[C])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) =>
        SerializationSuccess(AstNull())

      case TypeTagged(value) =>
        val errorsBuilder = immutable.Seq.newBuilder[SerializationError]

        val ast = AstObject[AST, S] { appendField =>
          if (typeMembers.nonEmpty) {
            import scala.collection.mutable

            val setsOfTypeArgsByTypeParam = new mutable.HashMap[Symbol, mutable.HashSet[Type]]

            for (fieldMember <- fieldMembers) {
              val TypeTagged(fieldValue) = fieldMember.valueIn(TypeTagged.inferFromRuntimeClass[C](value))
              val declaredFieldValueType = fieldMember.declaredValueType
              val actualFieldValueType = Reflection.inferTypeOf(fieldValue)(fieldMember.valueTypeTag)

              for (typeParam <- tpe.typeConstructor.typeParams) {
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
              val typeSerializationResult = typeSerializer.serialize(TypeTagged(ttt, TypeType))
              typeSerializationResult match {
                case SerializationSuccess(typeAst) =>
                  appendField(typeMember.name, typeAst)
                case SerializationFailure(typeErrors) =>
                  errorsBuilder ++= typeErrors
              }
            }

            val newType = appliedType(tpe.typeConstructor, typeArgs)
            val newTypeAdapter = context.typeAdapter(newType).as[ClassLikeTypeAdapter[C]]

            for (member <- newTypeAdapter.fieldMembers) {
              val valueSerializationResult = member.serializeValue(member.valueIn(tagged))
              valueSerializationResult match {
                case SerializationSuccess(valueAst) =>
                  appendField(member.name, valueAst)
                case SerializationFailure(valueErrors) =>
                  errorsBuilder ++= valueErrors
              }
            }
          } else {
            for (member <- fieldMembers) {
              val taggedMemberValue = member.valueIn(tagged)
              // FIXME              val memberName = mappedFieldsByName.get(member.name).map(_.fieldMapName.get).getOrElse(member.name)
              val memberName = member.name

              val valueSerializationResult = member.serializeValue(taggedMemberValue)

              valueSerializationResult match {
                case SerializationSuccess(memberValueAst) =>
                  appendField(memberName, memberValueAst)
                case failure @ SerializationFailure(_) if failure.isNothing =>
                // Nothing to do here
                case SerializationFailure(valueErrors) =>
                  errorsBuilder ++= valueErrors
              }
            }
          }

          value match {
            case sjc: SJCapture =>
              sjc.captured.map { cap =>
                cap.astOps.foreachObjectField(cap.capturedFields.asInstanceOf[cap.astOps.ObjectFields], { (memberName, memberValue) =>
                  appendField(memberName, AstValue.transform[cap.ASTType, AST, cap.SrcType, S](memberValue)(cap.astOps, ops))
                })
              }
            case _ =>
          }
        }

        val errors = errorsBuilder.result()

        if (errors.nonEmpty) {
          SerializationFailure(errors)
        } else {
          SerializationSuccess(handleDBKeys(ast, fieldMembers))
        }
    }

}