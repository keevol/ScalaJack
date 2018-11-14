package co.blocke.scalajack
package typeadapter

import scala.reflect.ClassTag

class TermIRTransceiver[T](next: IRTransceiver[T])(implicit tt: TypeTag[T]) extends IRTransceiver[T] {

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[T] =
    next.read(path, ir) match {
      case success @ ReadSuccess(_) =>
        success

      case failure @ ReadFailure(_) if failure.isUnsupported(path) =>
        ir match {
          case IRString(termName) =>
            if (tt.tpe.typeSymbol.isClass) {
              val classSymbol: ClassSymbol = tt.tpe.typeSymbol.asClass
              val ownerType: Type = classSymbol.owner.typeSignature
              // Perhaps the term refers to a sibling type?
              val siblingSymbol: Symbol = ownerType.member(TermName(termName))
              if (siblingSymbol.isModule) {
                val st = SingleType(ownerType, siblingSymbol)
                val siblingModule = reflectModule(siblingSymbol.asModule).instance
                return ReadSuccess(TypeTagged[T](siblingModule.asInstanceOf[T], st))
              }
            }
            failure

          case _ => failure
        }

      case failure @ ReadFailure(_) => failure
    }

  override def readFromNothing[IR, WIRE](path: Path)(implicit ops: Ops[IR, WIRE]): ReadResult[T] =
    next.readFromNothing(path)

  override def write[IR, WIRE](tagged: TypeTagged[T])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    next.write(tagged) // TODO implement this method
}

object TermTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    val ta = next.typeAdapterOf[T]
    val termTransceiver = new TermIRTransceiver[T](ta.irTransceiver)
    new TermTypeAdapter[T](termTransceiver, ta)
  }

}

class TermTypeAdapter[T](override val irTransceiver: IRTransceiver[T], val next: TypeAdapter[T]) extends TypeAdapter[T] {

  self =>

  // ----Apparently not used/needed!
  //  override def as[U <: TypeAdapter[_]: ClassTag]: U =
  //    maybeAs[U].getOrElse(throw new RuntimeException(s"Neither $self nor $next is an instance of ${implicitly[ClassTag[U]].runtimeClass}"))

  override def maybeAs[U <: TypeAdapter[_]: ClassTag]: Option[U] =
    super.maybeAs orElse next.maybeAs[U]

}