package co.blocke.scalajack
package typeadapter

object EitherTypeAdapter extends TypeAdapterFactory.=:=.withTwoTypeParams[Either] {

  override def create[L, R](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[Either[L, R]], ttLeft: TypeTag[L], ttRight: TypeTag[R]): TypeAdapter[Either[L, R]] = {
    val leftType = ttLeft.tpe
    val rightType = ttRight.tpe

    if (leftType <:< rightType || rightType <:< leftType) {
      throw new IllegalArgumentException(s"Types $leftType and $rightType are not mutually exclusive")
    }

    val leftTypeAdapter = context.typeAdapterOf[L]
    val rightTypeAdapter = context.typeAdapterOf[R]

    EitherTypeAdapter(
      new EitherIRTransceiver(leftTypeAdapter.irTransceiver, rightTypeAdapter.irTransceiver),
      leftTypeAdapter,
      rightTypeAdapter)
  }

}

case class EitherTypeAdapter[L, R](override val irTransceiver: IRTransceiver[Either[L, R]], leftTypeAdapter: TypeAdapter[L], rightTypeAdapter: TypeAdapter[R]) extends TypeAdapter[Either[L, R]]

case class EitherIRTransceiver[L, R](leftTransciever: IRTransceiver[L], rightTransceiver: IRTransceiver[R])(implicit ttLeft: TypeTag[L], ttRight: TypeTag[R]) extends IRTransceiver[Either[L, R]] {

  private val leftTypeConstructor: Type = typeOf[Left[_, _]].typeConstructor
  private val rightTypeConstructor: Type = typeOf[Right[_, _]].typeConstructor

  private val defaultLeftValueType: Type = ttLeft.tpe
  private val defaultRightValueType: Type = ttRight.tpe

  private class TaggedLeft(override val get: Left[L, R], taggedLeftValue: TypeTagged[L]) extends TypeTagged[Left[L, R]] {
    override lazy val tpe: Type = appliedType(leftTypeConstructor, taggedLeftValue.tpe, defaultRightValueType)
  }

  private class TaggedRight(override val get: Right[L, R], taggedRightValue: TypeTagged[R]) extends TypeTagged[Right[L, R]] {
    override lazy val tpe: Type = appliedType(rightTypeConstructor, defaultLeftValueType, taggedRightValue.tpe)
  }

  private val EitherSymbol: Symbol = symbolOf[Either[_, _]]

  private class TaggedLeftValue(override val get: L, taggedEither: TypeTagged[Either[L, R]]) extends TypeTagged[L] {
    override lazy val tpe: Type = {
      val leftType :: _ :: Nil = taggedEither.tpe.baseType(EitherSymbol).typeArgs
      leftType
    }
  }

  private class TaggedRightValue(override val get: R, taggedEither: TypeTagged[Either[L, R]]) extends TypeTagged[R] {
    override lazy val tpe: Type = {
      val _ :: rightType :: Nil = taggedEither.tpe.baseType(EitherSymbol).typeArgs
      rightType
    }
  }

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Either[L, R]] =
    rightTransceiver.read(path, ir) match {
      case ReadSuccess(taggedRightValue @ TypeTagged(rightValue)) =>
        ReadSuccess(new TaggedRight(Right(rightValue), taggedRightValue))

      case ReadFailure(rightErrors) =>
        leftTransciever.read(path, ir) match {
          case ReadSuccess(taggedLeftValue @ TypeTagged(leftValue)) =>
            ReadSuccess(new TaggedLeft(Left(leftValue), taggedLeftValue))

          case ReadFailure(leftErrors) =>
            ReadFailure(rightErrors ++ leftErrors)
        }
    }

  override def write[IR, WIRE](taggedEither: TypeTagged[Either[L, R]])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    taggedEither match {
      case TypeTagged(Left(leftValue)) =>
        leftTransciever.write(new TaggedLeftValue(leftValue, taggedEither))

      case TypeTagged(Right(rightValue)) =>
        rightTransceiver.write(new TaggedRightValue(rightValue, taggedEither))

      case TypeTagged(null) => WriteSuccess(IRNull())

    }
}