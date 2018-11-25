package co.blocke.scalajack
package dynamodb

import org.json4s.JsonAST.JValue

import scala.reflect.runtime.universe.Type
import scala.collection.JavaConverters._
import typeadapter._
import com.amazonaws.services.dynamodbv2.document.Item
import com.amazonaws.services.dynamodbv2.model.{ AttributeDefinition, CreateTableRequest, KeySchemaElement, KeyType, ProvisionedThroughput, ScalarAttributeType }

case class DynamoFlavor(
    customAdapters:    List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    hintMap:           Map[Type, String]        = Map.empty[Type, String],
    hintModifiers:     Map[Type, HintModifier]  = Map.empty[Type, HintModifier],
    typeModifier:      Option[HintModifier]     = None,
    parseOrElseMap:    Map[Type, Type]          = Map.empty[Type, Type],
    defaultHint:       String                   = "_hint",
    isCanonical:       Boolean                  = true,
    secondLookParsing: Boolean                  = false) extends ScalaJackLike[JValue, Item] {

  def withAdapters(ta: TypeAdapterFactory*) = this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withHints(h: (Type, String)*) = this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintModifier)*) = this.copy(hintModifiers = this.hintModifiers ++ hm)
  def withDefaultHint(hint: String) = this.copy(defaultHint = hint)
  def withTypeModifier(tm: HintModifier) = this.copy(typeModifier = Some(tm))
  def parseOrElse(poe: (Type, Type)*) = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def withSecondLookParsing() = throw new java.lang.UnsupportedOperationException("Not available for Dynamo formatting")
  def isCanonical(canonical: Boolean) = throw new UnsupportedOperationException("Not available for Dynamo formatting")

  // Embedded JSON-flavored ScalaJack, as Item can read/write JSON, so this is actually the most straightforward
  // path to serialization.
  lazy val sj = {
    val baseSj = ScalaJack()
      .withAdapters(customAdapters: _*)
      .withHints(hintMap.toList: _*)
      .withHintModifiers(hintModifiers.toList: _*)
      .withDefaultHint(defaultHint)
      .parseOrElse(parseOrElseMap.toList: _*)
    typeModifier.map(tm => baseSj.withTypeModifier(tm)).getOrElse(baseSj)
  }

  implicit val guidance: SerializationGuidance = SerializationGuidance().withMapValue()
  implicit val ops: Ops[JValue, Item] = DynamoOps.asInstanceOf[Ops[JValue, Item]]

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): Item =
    Item.fromJSON(sj.render(value))

  // This is Dynamo-Only.  User will have to cir ScalaJack to DynamoFlavor to call this.
  def createTableRequest[T](provisionedThroughput: ProvisionedThroughput)(implicit tt: TypeTag[T]): CreateTableRequest = {
    val tpe = tt.tpe
    val ta = context.typeAdapterOf[T].as[ClassLikeTypeAdapter[T]]

    val tableName = ta.collectionName.getOrElse(
      throw new java.lang.IllegalStateException(s"Class ${tpe.typeSymbol.fullName} must be annotated with @Collection to specify a table name."))

    val keys = ta.dbKeys
    if (keys.isEmpty) throw new java.lang.IllegalStateException(s"Class ${tpe.typeSymbol.fullName} must define at least a primary key with @DBKey.")
    val attrDetail = keys.zipWithIndex.collect {
      case (key, idx) if (idx == 0) => (new AttributeDefinition(key.name, getAttrType(key)), new KeySchemaElement(key.name, KeyType.HASH))
      case (key, idx) if (idx == 1) => (new AttributeDefinition(key.name, getAttrType(key)), new KeySchemaElement(key.name, KeyType.RANGE))
    }
    new CreateTableRequest(attrDetail.map(_._1).asJava, tableName, attrDetail.map(_._2).asJava, provisionedThroughput)
  }

  private def getAttrType(key: ClassLikeTypeAdapter.FieldMember[_]) =
    if (key.valueType.typeSymbol.asClass.isNumeric)
      ScalarAttributeType.N
    else
      ScalarAttributeType.S

}
