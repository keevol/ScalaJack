package co.blocke.scalajack
package typeadapter

class OptionIRTransceiver[T](next: IRTransceiver[T])(implicit tt: TypeTag[T]) extends IRTransceiver[Option[T]] {

  private val SomeTypeConstructor: Type = typeOf[Some[_]].typeConstructor
  private val TaggedNone: TypeTagged[None.type] = TypeTagged(None, typeOf[None.type])
  private val TaggedNull: TypeTagged[None.type] = TypeTagged(null.asInstanceOf[None.type], typeOf[None.type])

  private class TaggedSome(override val get: Some[T], taggedValue: TypeTagged[T]) extends TypeTagged[Some[T]] {
    override lazy val tpe: Type = appliedType(SomeTypeConstructor, taggedValue.tpe)
  }

  private val OptionTypeSymbol: TypeSymbol = symbolOf[Option[_]]

  private class TaggedSomeValue(override val get: T, taggedOption: TypeTagged[Option[T]]) extends TypeTagged[T] {
    override lazy val tpe: Type = taggedOption.tpe.baseType(OptionTypeSymbol).typeArgs.head
  }

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Option[T]] =
    ir match {
      case IRNull() if (guidance.isMapKey) =>
        ReadSuccess(TaggedNull)
      case IRNull() =>
        ReadSuccess(TaggedNone)
      case IRString(s) if (s == "") =>
        // Handle empty string.  If T is String type then conjure up Some("") else morph "" into None
        typeOf[T] match {
          case t if t == typeOf[String] && !guidance.isMapKey =>
            next.read(path, ir) map {
              case tagged @ TypeTagged(value) =>
                Option(value) match {
                  case None =>
                    TaggedNone
                  case some @ Some(_) =>
                    new TaggedSome(some, tagged)
                }
            }
          case _ => ReadSuccess(TaggedNone)
        }
      case _ =>
        next.read(path, ir) map {
          case tagged @ TypeTagged(value) =>
            Option(value) match {
              case None =>
                TaggedNone
              case some @ Some(_) =>
                new TaggedSome(some, tagged)
            }
        }
    }

  override def readFromNothing[IR, WIRE](path: Path)(implicit ops: Ops[IR, WIRE]): ReadResult[Option[T]] =
    ReadSuccess(TaggedNone)

  override def write[IR](tagged: TypeTagged[Option[T]])(implicit ops: OpsBase[IR], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(null) => WriteSuccess(IRNull())
      case TypeTagged(None) if (guidance.isMapKey) => WriteSuccess(IRString(""))
      case TypeTagged(None) if (guidance.inSeq || guidance.isMapValue) => WriteSuccess(IRNull())
      case TypeTagged(None) => WriteFailure(WriteError.Nothing)
      case TypeTagged(Some(value)) => next.write(new TaggedSomeValue(value, tagged))
    }
}
