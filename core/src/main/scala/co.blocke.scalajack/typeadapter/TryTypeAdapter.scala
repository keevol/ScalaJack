package co.blocke.scalajack
package typeadapter

import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

object TryTypeAdapter extends TypeAdapterFactory.=:=.withOneTypeParam[Try] {

  override def create[E](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[Try[E]], ttElement: TypeTag[E]): TypeAdapter[Try[E]] = {
    val valueTypeAdapter = context.typeAdapterOf[E]
    TryTypeAdapter(
      new TryIRTransceiver[E](valueTypeAdapter.irTransceiver),
      valueTypeAdapter)
  }

}

case class TryTypeAdapter[T](override val irTransceiver: IRTransceiver[Try[T]], valueTypeAdapter: TypeAdapter[T]) extends TypeAdapter[Try[T]]

class TryIRTransceiver[T](next: IRTransceiver[T])(implicit tt: TypeTag[T]) extends IRTransceiver[Try[T]] {

  private val successTypeConstructor: Type = typeOf[Success[_]].typeConstructor
  private val failureType: Type = appliedType(typeOf[Failure[_]].typeConstructor, tt.tpe)
  private val TryTypeSymbol: TypeSymbol = symbolOf[Try[_]]

  private class TaggedSuccess(override val get: Success[T], taggedValue: TypeTagged[T]) extends TypeTagged[Success[T]] {
    override lazy val tpe: Type = appliedType(successTypeConstructor, taggedValue.tpe)
  }
  private class TaggedSuccessValue(override val get: T, taggedTry: TypeTagged[Try[T]]) extends TypeTagged[T] {
    override lazy val tpe: Type = taggedTry.tpe.baseType(TryTypeSymbol).typeArgs.head
  }

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Try[T]] =
    try {
      val readResult = next.read(path, ir) map {
        case tagged @ TypeTagged(value) =>
          new TaggedSuccess(Success(value), tagged)
      }

      readResult match {
        case ReadSuccess(_) => readResult

        case readFailure @ ReadFailure(_) =>
          val exceptionWithBackingIRValue = new ReadException(readFailure) with BackedByIRValue {
            override type BackingIRValue = IR
            override val backingIRValue: BackingIRValue = ir
            override val backingOps: OpsBase[BackingIRValue] = ops
          }
          ReadSuccess(TypeTagged(Failure(exceptionWithBackingIRValue), failureType))
      }
    } catch {
      case exception: ReadException with BackedByIRValue =>
        ReadSuccess(TypeTagged(Failure(exception), failureType))

      case exception: ReadException =>
        val exceptionWithBackingIRValue = new ReadException(exception.readFailure) with BackedByIRValue {
          override type BackingIRValue = IR
          override val backingIRValue: BackingIRValue = ir
          override val backingOps: OpsBase[BackingIRValue] = ops
        }
        ReadSuccess(TypeTagged(Failure(exceptionWithBackingIRValue), failureType))

      case NonFatal(e) =>
        ReadSuccess(TypeTagged(Failure(e), failureType))
    }

  override def write[IR, WIRE](tagged: TypeTagged[Try[T]])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(Success(value))              => next.write(new TaggedSuccessValue(value, tagged))
      case TypeTagged(Failure(e: BackedByIRValue)) => WriteSuccess(e.backingAstValueAs[IR])
      case TypeTagged(Failure(e))                  => throw e
    }
}

trait BackedByIRValue {

  type BackingIRValue

  def backingIRValue: BackingIRValue
  def backingAstValueAs[IR]()(implicit ops: OpsBase[IR]): IR = backingOps.become[IR](backingIRValue)
  implicit def backingOps: OpsBase[BackingIRValue]

}