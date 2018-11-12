package co.blocke.scalajack
package typeadapter
package javaprimitives

class BoxedShortDeserializer(shortDeserializer: Deserializer[Short]) extends Deserializer[java.lang.Short] {

  private val BoxedShortType: Type = typeOf[java.lang.Short]

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[java.lang.Short] =
    ast match {
      case AstNull() =>
        DeserializationSuccess(TypeTagged(null, BoxedShortType))

      case _ =>
        shortDeserializer.deserialize(path, ast) map {
          case TypeTagged(shortValue) => TypeTagged(java.lang.Short.valueOf(shortValue), BoxedShortType)
        }
    }

}
