package co.blocke.scalajack
package typeadapter

import scala.reflect.api.{ Mirror, Universe }
import scala.reflect.runtime.{ currentMirror, universe }
import java.lang.reflect.Method

object ClassLikeTypeAdapter {

  sealed trait Member[Owner] {
    def name: MemberName
  }

  trait TypeMember[Owner] extends Member[Owner]

  trait FieldMember[Owner] extends Member[Owner] {

    type Value

    // Case class and Plain class
    val valueType: Type
    val valueTypeAdapter: TypeAdapter[Value]
    val derivedValueClassConstructorMirror: Option[MethodMirror]
    val outerClass: Option[java.lang.Class[_]]
    val defaultValue: Option[Value]
    val valueAccessorMethod: Method
    val dbKeyIndex: Option[Int]

    def declaredValueType: Type

    // Conveniences
    lazy val isOptional = valueTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]
    def deserializer: Deserializer[Value] = valueTypeAdapter.deserializer
    def serializer: Serializer[Value] = valueTypeAdapter.serializer

    def deserializeValue[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Value] =
      valueTypeAdapter.deserializer.deserialize(path, ast)
    def deserializeValueFromNothing[AST, S](path: Path)(implicit ops: AstOps[AST, S]): DeserializationResult[Value] =
      valueTypeAdapter.deserializer.deserializeFromNothing(path)
    def serializeValue[AST, S](tagged: TypeTagged[Value])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
      valueTypeAdapter.serializer.serialize(tagged)

    def valueIn(tagged: TypeTagged[Owner]): TypeTagged[Value] = {
      val TypeTagged(owner) = tagged
      val value = valueAccessorMethod.invoke(owner)

      if (outerClass.isEmpty || outerClass.get.isInstance(value)) {
        TypeTagged(value.asInstanceOf[Value], valueType)
      } else {
        derivedValueClassConstructorMirror match {
          case Some(methodMirror) =>
            TypeTagged(methodMirror.apply(value).asInstanceOf[Value], valueType)

          case None =>
            TypeTagged(value.asInstanceOf[Value], valueType)
        }
      }
    }

    lazy val valueTypeTag = new TypeTag[Value] {
      override def in[U <: Universe with Singleton](otherMirror: Mirror[U]): U#TypeTag[Value] = ???
      override val mirror: universe.Mirror = currentMirror
      override def tpe: universe.Type = valueType
    }
  }
}

trait ClassLikeTypeAdapter[C] extends TypeAdapter[C] {

  val typeMembers: List[ClassLikeTypeAdapter.TypeMember[C]]
  val fieldMembers: List[ClassLikeTypeAdapter.FieldMember[C]]
  val collectionName: Option[String]

  def dbKeys: List[ClassLikeTypeAdapter.FieldMember[C]] = fieldMembers.filter(_.dbKeyIndex.isDefined).sortBy(_.dbKeyIndex.get)
  def members = typeMembers ++ fieldMembers
}
