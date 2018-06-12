package co.blocke.scalajack
package typeadapter
package javacollections

class JavaCollectionSerializer[E, C <: java.util.Collection[E]](elementSerializer: Serializer[E]) extends Serializer[C] {

  override def serialize[J](taggedCollection: TypeTagged[C])(implicit ops: JsonOps[J]): SerializationResult[J] =
    taggedCollection match {
      case TypeTagged(collection) =>
        lazy val elementType: Type = {
          val e :: Nil = taggedCollection.tpe.baseType(symbolOf[java.util.Collection[_]]).typeArgs
          e
        }

        class TaggedElement(override val get: E) extends TypeTagged[E] {
          override def tpe: Type = elementType
        }

        SerializationSuccess(JsonArray { appendElement =>
          val i = collection.iterator()
          while (i.hasNext) {
            val element = i.next()
            val SerializationSuccess(elementJson) = elementSerializer.serialize(new TaggedElement(element))
            appendElement(elementJson)
          }
        })
    }

}