package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedBooleanDeserializer(booleanDeserializer: Deserializer[Boolean]) extends Deserializer[java.lang.Boolean] {

  private val BoxedBooleanType: Type = typeOf[java.lang.Boolean]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[java.lang.Boolean] =
    ast match {
      case AstNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedBooleanType))

      case _ =>
        booleanDeserializer.deserialize(path, ast) map {
          case TypeTagged(booleanValue) => TypeTagged(java.lang.Boolean.valueOf(booleanValue), BoxedBooleanType)
        }
    }

}