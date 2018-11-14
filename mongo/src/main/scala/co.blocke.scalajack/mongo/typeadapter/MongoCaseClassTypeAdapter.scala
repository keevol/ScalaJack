package co.blocke.scalajack
package mongo
package typeadapter

import co.blocke.scalajack.typeadapter.{ CaseClassTypeAdapter, ClassReaderUsingReflectedConstructor }

object MongoCaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  val IdMemberName: MemberName = "_id"

  // Some magic because Tuples are also case classes, but they have their own type adapter, and we *don't*
  // want the case class machinery to kick in for Tuples.
  val tupleFullName = """scala.Tuple(\d+)""".r

  /*
  For Mongo case classes, re-use all the core case class machinery except we need to force Mongo-specific serializer/deserializer to handle
  the DBKey representation.  This approach hijack's the normal CaseClassTypeAdapter (which is a factory) and takes the generated
  CaseClassTypeAdapter (instance) and re-wires it to Mongo-specific ser/deser.
   */
  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isCaseClass) {
      classSymbol.fullName match {
        case tupleFullName(_) =>
          next.typeAdapterOf[T]
        case _ =>
          val ccta = next.typeAdapterOf[T].as[CaseClassTypeAdapter[T]]
          val constructorMirror = ccta.deserializer.asInstanceOf[ClassReaderUsingReflectedConstructor[T]].constructorMirror

          ccta.copy(
            serializer   = new MongoCaseClassSerializer(
              ccta.dbKeys,
              IdMemberName,
              context,
              context.typeAdapterOf[Type].serializer,
              ccta.typeMembers.asInstanceOf[List[CaseClassTypeAdapter.TypeMember[T]]],
              ccta.fieldMembers,
              false),
            deserializer = new MongoCaseClassDeserializer(
              ccta.dbKeys,
              IdMemberName,
              context,
              constructorMirror,
              context.typeAdapterOf[Type].deserializer,
              ccta.typeMembers.asInstanceOf[List[CaseClassTypeAdapter.TypeMember[T]]],
              ccta.fieldMembers,
              false))
      }
    } else
      next.typeAdapterOf[T]
}
