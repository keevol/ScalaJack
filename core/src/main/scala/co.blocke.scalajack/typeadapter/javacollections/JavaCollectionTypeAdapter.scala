package co.blocke.scalajack
package typeadapter
package javacollections

import scala.collection.JavaConverters._

object JavaCollectionTypeAdapter extends TypeAdapterFactory.<:<.withOneTypeParam[java.util.Collection] {

  override def create[E, G <: java.util.Collection[E]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[G], ttCollection: TypeTag[java.util.Collection[E]], ttElement: TypeTag[E]): TypeAdapter[G] = {
    val collectionConstructor: java.lang.reflect.Constructor[G] = runtimeClassOf[G].getConstructor()

    def newEmptyCollection(): G = collectionConstructor.newInstance()

    val elementTypeAdapter = context.typeAdapterOf[E]

    new JavaCollectionTypeAdapter[E, G](
      new IRTransceiver[G] {

        self =>

        private val collectionType: Type = tt.tpe
        private val collectionTypeConstructor: Type = tt.tpe
        private val nullTypeTagged: TypeTagged[G] = TypeTagged(null.asInstanceOf[G], collectionType)

        private class TaggedCollection(override val get: G, taggedElements: List[TypeTagged[E]]) extends TypeTagged[G] {
          override lazy val tpe: Type = appliedType(collectionTypeConstructor, taggedElements.map(_.tpe)) // FIXME `C` may not actually have a type parameter.
        }

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[G] =
          ir match {
            case IRNull() =>
              ReadSuccess(nullTypeTagged)

            case IRArray(elements) =>
              ReadResult(path) {
                val collection: G = newEmptyCollection()
                val taggedElementsBuilder = List.newBuilder[TypeTagged[E]]
                elements.zipWithIndex.foreach {
                  case (elementIr, index) =>
                    val ReadSuccess(taggedElement) = elementTypeAdapter.irTransceiver.read(path \ index, elementIr)
                    val TypeTagged(element) = taggedElement
                    taggedElementsBuilder += taggedElement
                    collection.add(element)
                }
                val taggedElements = taggedElementsBuilder.result()
                new TaggedCollection(collection, taggedElements)
              }

            case _ =>
              ReadFailure(path, ReadError.Unsupported(s"Expected a JSON array, not $ir", reportedBy = self))
          }

        override def write[IR, WIRE](tagged: TypeTagged[G])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          tagged match {
            case TypeTagged(null) =>
              WriteSuccess(IRNull())

            case TypeTagged(collection) =>
              lazy val elementType: Type = {
                val e :: Nil = tagged.tpe.baseType(symbolOf[java.util.Collection[_]]).typeArgs
                e
              }

              class TaggedElement(override val get: E) extends TypeTagged[E] {
                override def tpe: Type = elementType
              }

              WriteSuccess(IRArray(collection.iterator().asScala.toList.map(e => elementTypeAdapter.irTransceiver.write(new TaggedElement(e)).get)))
          }

      })
  }

}

class JavaCollectionTypeAdapter[E, G <: java.util.Collection[E]](override val irTransceiver: IRTransceiver[G]) extends TypeAdapter[G]