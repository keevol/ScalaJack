package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.Annotation

object ClassLikeTypeAdapter {

  sealed trait Member[Owner] {
    def name: MemberName
  }

  trait TypeMember[Owner] extends Member[Owner]

  /*
  trait ClassFieldMember[Owner] extends ClassLikeTypeAdapter.FieldMember[Owner] {
    def dbKeyIndex: Option[Int]
    def declaredValueType: Type
  }

  object CaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {
    case class TypeMember[Owner](name: MemberName, typeSignature: Type, baseType: Type) extends ClassLikeTypeAdapter.TypeMember[Owner]

    case class FieldMember[Owner, T](
                                      index:                              Int,
                                      name:                               MemberName,
                                      valueType:                          Type,
                                      valueTypeAdapter:                   TypeAdapter[T],
                                      declaredValueType:                  Type,
                                      valueAccessorMethodSymbol:          MethodSymbol,
                                      valueAccessorMethod:                Method,
                                      derivedValueClassConstructorMirror: Option[MethodMirror],
                                      defaultValueMirror:                 Option[MethodMirror],
                                      outerClass:                         Option[java.lang.Class[_]],
                                      dbKeyIndex:                         Option[Int],
                                      fieldMapName:                       Option[String],
                                      annotations:                        List[universe.Annotation]) extends ClassFieldMember[Owner] {


  trait PlainFieldMember[Owner] extends ClassFieldMember[Owner] {
    implicit def ownerClassTag: ClassTag[Owner]
    val valueType: Type
    val valueTypeAdapter: TypeAdapter[Value]
    val valueGetterMethod: Method
    val valueSetterMethodSymbol: Option[MethodSymbol] // for Scala
    val valueSetterMethod: Option[Method] // for Java
    val derivedValueClassConstructorMirror: Option[MethodMirror]
    val outerClass: Option[java.lang.Class[_]]
    def deserializer: Deserializer[Value] = valueTypeAdapter.deserializer
    def serializer: Serializer[Value] = valueTypeAdapter.serializer
    lazy val isOptional = valueTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]
    override lazy val valueTypeTag = new TypeTag[Value] {
    def valueIn(tagged: TypeTagged[Owner]): TypeTagged[Value] =
    def valueSet(instance: Owner, value: Value): Unit =
    override def deserializeValueFromNothing[AST, S](path: Path)(implicit ops: AstOps[AST, S]): DeserializationResult[Value] =
    override def deserializeValue[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Value] =
    override def serializeValue[AST, S](tagged: TypeTagged[Value])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    override def defaultValue: Option[Value] = if (isOptional) {
    override def annotationOf[N](implicit tt: TypeTag[N]): Option[universe.Annotation] = None
    override def isStringValue: Boolean =

*/

  trait FieldMember[Owner] extends Member[Owner] {

    type Value

    def valueTypeTag: TypeTag[Value]
    def defaultValue: Option[Value]
    def valueIn(owner: TypeTagged[Owner]): TypeTagged[Value]
    def deserializeValueFromNothing[AST, S](path: Path)(implicit ops: AstOps[AST, S]): DeserializationResult[Value]
    def deserializeValue[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Value]
    def serializeValue[AST, S](tagged: TypeTagged[Value])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST]

    //
    //    def index: Int
    //
    //    def annotationOf[A](implicit tt: TypeTag[A]): Option[Annotation]
    //
    //    def isStringValue: Boolean

  }

}

trait ClassLikeTypeAdapter[C] extends TypeAdapter[C] {

  //  type TypeMember = ClassLikeTypeAdapter.TypeMember[C]
  type FieldMember = ClassLikeTypeAdapter.FieldMember[C]

  val members: List[ClassLikeTypeAdapter.Member[C]]
  val fieldMembers: List[FieldMember]

  val dbKeys: List[ClassFieldMember[C]] = ??? //fieldMembers.filter(_.dbKeyIndex.isDefined).sortBy(_.dbKeyIndex.get)

  //
  //  def members = typeMembers ++ fieldMembers
  //  def typeMembers: List[TypeMember]
  //  def typeMember(memberName: MemberName): Option[TypeMember]
  //  def fieldMember(memberName: MemberName): Option[FieldMember]
}
