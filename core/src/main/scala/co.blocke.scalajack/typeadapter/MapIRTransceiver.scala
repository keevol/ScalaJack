package co.blocke.scalajack
package typeadapter

import scala.collection.{ GenMap, immutable, mutable }
import scala.util.control.NonFatal

class MapIRTransceiver[K, V, M <: GenMap[K, V]](
    val keyTransceiver:      IRTransceiver[K],
    val valueTransceiver:    IRTransceiver[V],
    keyValuePairTransceiver: IRTransceiver[List[(K, V)]],
    newBuilder:              () => mutable.Builder[(K, V), M])(implicit tt: TypeTag[M], ttk: TypeTag[K], ttv: TypeTag[V], context: Context) extends IRTransceiver[M] {

  self =>

  private val taggedNull: TypeTagged[M] = TypeTagged(null.asInstanceOf[M], tt.tpe)

  private class TaggedMapFromIRArray(override val get: M) extends TypeTagged[M] {
    override lazy val tpe: Type = appliedType(tt.tpe.typeConstructor, typeOf[K], typeOf[V]) // TODO `M` may not actually have type parameters
  }

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[M] =
    ir match {
      case IRNull() =>
        ReadSuccess(taggedNull)

      // Comes in raw from some WIRE formats, e.g. JSON, that require String-typed keys
      case IRObject(objectFields) =>
        try {
          val builder = newBuilder()
          val taggedKeysBuilder = List.newBuilder[TypeTagged[K]]
          val taggedValuesBuilder = List.newBuilder[TypeTagged[V]]

          val errorsBuilder = immutable.Seq.newBuilder[(Path, ReadError)]

          objectFields.foreach {
            case (fieldName, fieldValueIR) =>
              val keyReadResult = keyTransceiver.read(path \ fieldName, IRString(fieldName))(ops, guidance.withMapKey())
              val valueReadResult = valueTransceiver.read(path \ fieldName, fieldValueIR)(ops, guidance.withMapValue())

              //            println("K: " + keyDeserializationResult)
              //            println("V: " + valueDeserializationResult)
              //            println("--------------")
              (keyReadResult, valueReadResult) match {
                case (ReadSuccess(taggedKey), ReadSuccess(taggedValue)) =>
                  val TypeTagged(key) = taggedKey
                  taggedKeysBuilder += taggedKey

                  val TypeTagged(value) = taggedValue
                  taggedValuesBuilder += taggedValue

                  builder += key -> value

                case _ =>
                  errorsBuilder ++= keyReadResult.errors
                  errorsBuilder ++= valueReadResult.errors
              }
          }

          val errors = errorsBuilder.result()

          if (errors.nonEmpty) {
            ReadFailure(errors)
          } else {
            val map = builder.result()

            class TaggedMapFromIRObject(override val get: M, taggedKeys: List[TypeTagged[K]], taggedValues: List[TypeTagged[V]]) extends TypeTagged[M] {
              override lazy val tpe: Type = tt.tpe
            }

            ReadSuccess(new TaggedMapFromIRObject(map, taggedKeysBuilder.result(), taggedValuesBuilder.result()))
          }
        } catch {
          case NonFatal(e) =>
            ReadFailure(path, ReadError.ExceptionThrown(e))
        }

      case IRMap(fields) =>
        ReadResult(path) {
          val builder = newBuilder()
          fields.map {
            case (kIR, vIR) =>
              builder += ((keyTransceiver.read(path, kIR).get, valueTransceiver.read(path, vIR).get))
          }
          val map = builder.result()
          new TaggedMapFromIRArray(map)
        }

      case IRArray(_) =>
        ReadResult(path) {
          val ReadSuccess(taggedKeyValuePairs) = keyValuePairTransceiver.read(path, ir)
          val TypeTagged(keyValuePairs) = taggedKeyValuePairs

          lazy val keyValuePairType: Type = taggedKeyValuePairs.tpe.baseType(symbolOf[List[_]]).typeArgs.head

          lazy val keyType: Type = {
            val k :: _ :: Nil = keyValuePairType.baseType(symbolOf[(_, _)]).typeArgs
            k
          }

          lazy val valueType: Type = {
            val _ :: v :: Nil = keyValuePairType.baseType(symbolOf[(_, _)]).typeArgs
            v
          }

          class TaggedMapFromIRArray(override val get: M) extends TypeTagged[M] {
            override lazy val tpe: Type = appliedType(tt.tpe.typeConstructor, keyType, valueType) // TODO `M` may not actually have type parameters
          }

          val builder = newBuilder()
          builder ++= keyValuePairs
          val map = builder.result()

          new TaggedMapFromIRArray(map)
        }

      case IRString(s) if (guidance.isMapKey) => // Parse and deserialize non-string Map key (embedded in a string, e.g. Map as a key to another Map)
        context.typeAdapterOf[M].irTransceiver.read(Path.Root, ops.deserialize(path, s.asInstanceOf[WIRE]).get)

      case _ =>
        ReadFailure(path, ReadError.Unsupported("Expected a JSON object", reportedBy = self))
    }

  override def write[IR, WIRE](tagged: TypeTagged[M])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(null) => WriteSuccess(IRNull())
      case TypeTagged(map) =>
        lazy val mapType: Type = tagged.tpe.baseType(symbolOf[GenMap[_, _]])

        lazy val keyType: Type = {
          val k :: _ :: Nil = mapType.typeArgs
          k
        }

        lazy val valueType: Type = {
          val _ :: v :: Nil = mapType.typeArgs
          v
        }

        class TaggedKey(override val get: K) extends TypeTagged[K] {
          override def tpe: Type = keyType
        }

        class TaggedValue(override val get: V) extends TypeTagged[V] {
          override def tpe: Type = valueType
        }

        val errorsBuilder = immutable.Seq.newBuilder[WriteError]

        val fields = mutable.ListBuffer.empty[(IR, IR)]
        map foreach {
          case (key, value) =>
            val keyWriteResult = keyTransceiver.write(new TaggedKey(key))(ops, guidance.withMapKey())
            val valueWriteResult = valueTransceiver.write(new TaggedValue(value))(ops, guidance.withMapValue())

            (keyWriteResult, valueWriteResult) match {
              case (WriteSuccess(keyIR), WriteSuccess(valueIR)) =>
                fields += ((keyIR, valueIR))

              case (WriteSuccess(keyIR), WriteFailure(Seq(WriteError.Nothing))) =>
                fields += ((keyIR, IRNull()))

              case _ =>
                errorsBuilder ++= keyWriteResult.errors
                errorsBuilder ++= valueWriteResult.errors
            }
        }

        val errors = errorsBuilder.result()
        if (errors.nonEmpty) {
          WriteFailure(errors)
        } else {
          // We would normally think of representing a Map-kind as an IRObject, but...
          // IRObject presumes a String-based Map key.  That's true for JSON, but not in Scala generally.
          // Since we don't presume JSON in ScalaJack core, we can't use IRObject for Maps.  (We can for class
          // serialization, since field names are indeed Strings.)
          WriteSuccess(IRMap(fields))
        }
    }
}
