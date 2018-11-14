package co.blocke.scalajack
package typeadapter

object TypeParameterTypeAdapter extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.isParameter) {
      val anyTypeAdapter = context.typeAdapterOf[Any]
      TypeParameterTypeAdapter[T](anyTypeAdapter.irTransceiver.asInstanceOf[IRTransceiver[T]], anyTypeAdapter)
    } else {
      next.typeAdapterOf[T]
    }

}

case class TypeParameterTypeAdapter[T](override val irTransceiver: IRTransceiver[T], anyTypeAdapter: TypeAdapter[Any]) extends TypeAdapter[T]
