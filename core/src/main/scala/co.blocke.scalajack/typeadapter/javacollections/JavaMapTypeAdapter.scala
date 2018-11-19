package co.blocke.scalajack
package typeadapter
package javacollections

import scala.collection.JavaConverters._

object JavaMapTypeAdapter extends TypeAdapterFactory.<:<.withTwoTypeParams[java.util.Map] {

  override def create[K, V, M <: java.util.Map[K, V]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[M], ttMap: TypeTag[java.util.Map[K, V]], ttKey: TypeTag[K], ttValue: TypeTag[V]): TypeAdapter[M] = {

    val mapConstructor: java.lang.reflect.Constructor[M] = runtimeClassOf[M].getDeclaredConstructor()

    def newEmptyMap(): M = mapConstructor.newInstance()

    val keyTypeAdapter = context.typeAdapterOf[K]
    val valueTypeAdapter = context.typeAdapterOf[V]

    new JavaMapTypeAdapter[K, V, M](
      new IRTransceiver[M] {

        self =>

        private val taggedNull: TypeTagged[M] = TypeTagged(null.asInstanceOf[M], tt.tpe)
        private val mapSymbol: Symbol = symbolOf[java.util.Map[_, _]]

        override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[M] =
          ir match {
            case IRObject(objectFields) =>
              val map: M = newEmptyMap()
              objectFields.foreach {
                case (fieldName, fieldValue) =>
                  val ReadSuccess(TypeTagged(key)) = keyTypeAdapter.irTransceiver.read(path \ fieldName, IRString(fieldName))
                  val ReadSuccess(TypeTagged(value)) = valueTypeAdapter.irTransceiver.read(path \ fieldName, fieldValue)
                  map.put(key.asInstanceOf[K], value.asInstanceOf[V])
              }
              ReadSuccess(TypeTagged(map, tt.tpe))

            case IRNull() => ReadSuccess(taggedNull)

            case _ =>
              ReadFailure(path, ReadError.Unsupported("Expected a JSON object", reportedBy = self))
          }

        override def write[IR, WIRE](tagged: TypeTagged[M])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
          tagged match {
            case TypeTagged(null) =>
              WriteSuccess(IRNull())

            case TypeTagged(map) =>
              lazy val baseType: Type = tagged.tpe.baseType(mapSymbol)

              lazy val keyType: Type = {
                val k :: _ :: Nil = baseType.typeArgs
                k
              }

              lazy val valueType: Type = {
                val _ :: v :: Nil = baseType.typeArgs
                v
              }

              class TaggedKey(override val get: K) extends TypeTagged[K] {
                override def tpe: Type = keyType
              }

              class TaggedValue(override val get: V) extends TypeTagged[V] {
                override def tpe: Type = valueType
              }

              val unpacked = map.entrySet().iterator().asScala.toList.map { mapEntry =>
                val WriteSuccess(IRString(keyString)) = keyTypeAdapter.irTransceiver.write(new TaggedKey(mapEntry.getKey))
                val WriteSuccess(valueIr) = valueTypeAdapter.irTransceiver.write(new TaggedValue(mapEntry.getValue))
                (IRString(keyString), valueIr)
              }
              WriteSuccess(IRMap(unpacked))
          }

      })
  }
}

class JavaMapTypeAdapter[K, V, M <: java.util.Map[K, V]](override val irTransceiver: IRTransceiver[M]) extends TypeAdapter[M]
