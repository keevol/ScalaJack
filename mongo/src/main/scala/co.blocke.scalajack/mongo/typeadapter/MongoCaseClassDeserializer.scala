package co.blocke.scalajack
package mongo
package typeadapter

import co.blocke.scalajack.typeadapter._

class MongoCaseClassDeserializer[C](
    dbKeys:            List[ClassLikeTypeAdapter.FieldMember[_]],
    idMemberName:      String,
    context:           Context,
    constructorMirror: MethodMirror,
    typeDeserializer:  Deserializer[Type],
    typeMembers:       List[CaseClassTypeAdapter.TypeMember[C]],
    fieldMembers:      List[ClassLikeTypeAdapter.FieldMember[C]],
    isSJCapture:       Boolean)(implicit tt: TypeTag[C]) extends ClassReaderUsingReflectedConstructor[C](
  context, constructorMirror, typeDeserializer, typeMembers, fieldMembers, isSJCapture
) {

  override protected def handleDBKeys[AST, S](path: Path, ast: AST, members: List[ClassLikeTypeAdapter.FieldMember[C]])(implicit ops: AstOps[AST, S]): Either[DeserializationFailure, AST] =
    dbKeys.size match {
      case 0 => Right(ast)
      case 1 =>
        val keyFieldName = dbKeys.head.name
        var keyFound = false
        val resultAST = ops.mapObjectFields(ast.asInstanceOf[ops.ObjectFields], { (fieldname, value) =>
          fieldname match {
            case s: String if s == idMemberName =>
              keyFound = true
              (keyFieldName, value)
            case _ => (fieldname, value)
          }
        }).asInstanceOf[AST]
        if (keyFound)
          Right(resultAST)
        else
          Left(DeserializationFailure(path, ReadError.Unexpected(s"Did not find required db key field (e.g. _id)", this)))
      case _ =>
        val fields = scala.collection.mutable.ListBuffer.empty[(String, AST)]
        val keysFound = scala.collection.mutable.ListBuffer.empty[String]
        ops.foreachObjectField(ast.asInstanceOf[ops.ObjectFields], { (fieldname, element) =>
          if (fieldname == idMemberName) // expand id field if found
            ops.foreachObjectField(element.asInstanceOf[ops.ObjectFields], { (fieldname, element) => keysFound.append(fieldname); fields.append((fieldname, element)) })
          else
            fields.append((fieldname, element))
        })
        if (keysFound.size == dbKeys.size)
          Right(ops.applyObject(fields.toList))
        else {
          val missing = dbKeys.map(_.name).diff(keysFound).mkString("[", ",", "]")
          Left(DeserializationFailure(path, ReadError.Unexpected(s"Missing at least one required db key field (e.g. _id) component: $missing", this)))
        }
    }

}
