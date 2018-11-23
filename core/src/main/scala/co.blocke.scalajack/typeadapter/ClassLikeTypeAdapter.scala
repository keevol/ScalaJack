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
    def irTransceiver: IRTransceiver[Value] = valueTypeAdapter.irTransceiver

    def readValue[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Value] =
      irTransceiver.read(path, ir)
    def readValueFromNothing[IR, WIRE](path: Path)(implicit ops: Ops[IR, WIRE]): ReadResult[Value] =
      irTransceiver.readFromNothing(path)
    def writeValue[IR, WIRE](tagged: TypeTagged[Value])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
      irTransceiver.write(tagged)

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
            // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
            TypeTagged(value.asInstanceOf[Value], valueType)
            // $COVERAGE-ON$
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
