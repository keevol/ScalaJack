package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.FlexJsonFlavor.MemberName
import co.blocke.scalajack.flexjson.{ Context, ForwardingWriter, Reader, Reflection, TokenType, TypeAdapter, TypeAdapterFactory, Writer }

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, Type }

case class PolymorphicTypeAdapterFactory(hintFieldName: String) extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (classSymbol.isTrait) {
      Some(PolymorphicTypeAdapter(hintFieldName, context.typeAdapterOf[Type], context.typeAdapterOf[MemberName], context, tpe))
    } else {
      None
    }

}

class PolymorphicWriter(
    override val delegate: Writer,
    typeFieldName:         String,
    tpe:                   Type,
    typeTypeAdapter:       TypeAdapter[Type],
    memberNameTypeAdapter: TypeAdapter[MemberName]
) extends ForwardingWriter {

  var depth = 0

  override def beginObject(): Unit = {
    depth += 1
    super.beginObject()

    if (depth == 1) {
      memberNameTypeAdapter.write(typeFieldName, this)
      typeTypeAdapter.write(tpe, this)
    }
  }

  override def endObject(): Unit = {
    depth -= 1
    super.endObject()
  }

}

case class PolymorphicTypeAdapter[T](
    typeMemberName:        MemberName,
    typeTypeAdapter:       TypeAdapter[Type],
    memberNameTypeAdapter: TypeAdapter[MemberName],
    context:               Context,
    polymorphicType:       Type
) extends TypeAdapter[T] {

  import scala.collection.mutable

  val populatedConcreteTypeCache = new mutable.WeakHashMap[Type, Type]

  def populateConcreteType(concreteType: Type): Type =
    populatedConcreteTypeCache.getOrElseUpdate(concreteType, Reflection.populateChildTypeArgs(polymorphicType, concreteType))

  override def read(reader: Reader): T = {
    if (reader.peek == TokenType.Null) {
      reader.readNull().asInstanceOf[T]
    } else {
      val originalPosition = reader.position

      reader.beginObject()

      var optionalConcreteTypeBeforeSubstitution: Option[Type] = None

      while (optionalConcreteTypeBeforeSubstitution.isEmpty && reader.hasMoreMembers) {
        val memberName = memberNameTypeAdapter.read(reader)

        if (memberName == typeMemberName) {
          val concreteTypeBeforeSubstitution = typeTypeAdapter.read(reader)
          optionalConcreteTypeBeforeSubstitution = Some(concreteTypeBeforeSubstitution)
        } else {
          reader.skipValue()
        }
      }

      val concreteTypeBeforeSubstitution = optionalConcreteTypeBeforeSubstitution.getOrElse(throw new Exception(s"""Could not find type field named "$typeMemberName" """))
      val concreteTypeAfterSubstitution = populateConcreteType(concreteTypeBeforeSubstitution)
      val concreteTypeAdapter = context.typeAdapter(concreteTypeAfterSubstitution)

      reader.position = originalPosition

      concreteTypeAdapter.read(reader).asInstanceOf[T]
    }
  }

  override def write(value: T, writer: Writer): Unit = {
    val concreteTypeBeforeSubstitution = currentMirror.classSymbol(value.getClass).toType
    val concreteTypeAfterSubstitution = populateConcreteType(concreteTypeBeforeSubstitution)
    val valueTypeAdapter = context.typeAdapter(concreteTypeAfterSubstitution).asInstanceOf[TypeAdapter[T]]

    val polymorphicWriter = new PolymorphicWriter(writer, typeMemberName, concreteTypeAfterSubstitution, typeTypeAdapter, memberNameTypeAdapter)
    valueTypeAdapter.write(value, polymorphicWriter)
  }

}
