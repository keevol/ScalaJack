package co.blocke.scalajack
package mongo

import org.bson.BsonValue
import org.json4s.JsonAST.JValue
import typeadapter._

import scala.reflect.runtime.universe.Type

case class MongoFlavor(
    customAdapters:    List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    hintMap:           Map[Type, String]        = Map.empty[Type, String],
    hintModifiers:     Map[Type, HintModifier]  = Map.empty[Type, HintModifier],
    typeModifier:      Option[HintModifier]     = None,
    parseOrElseMap:    Map[Type, Type]          = Map.empty[Type, Type],
    defaultHint:       String                   = "_hint",
    isCanonical:       Boolean                  = true,
    secondLookParsing: Boolean                  = false) extends ScalaJackLike[JValue, BsonValue] with JackFlavor[JValue, BsonValue] {

  def withAdapters(ta: TypeAdapterFactory*) = this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withHints(h: (Type, String)*) = this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintModifier)*) = this.copy(hintModifiers = this.hintModifiers ++ hm)
  def withDefaultHint(hint: String) = this.copy(defaultHint = hint)
  def withTypeModifier(tm: HintModifier) = throw new UnsupportedOperationException("Not available for Mongo formatting")
  def parseOrElse(poe: (Type, Type)*) = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def isCanonical(canonical: Boolean) = throw new UnsupportedOperationException("Not available for Mongo formatting")
  def withSecondLookParsing() = this.copy(secondLookParsing = true)

  implicit val ops = BsonOps
  implicit val guidance: SerializationGuidance = SerializationGuidance()

  override def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): BsonValue = {
    val irTransceiver = context.typeAdapterOf[T].irTransceiver
    irTransceiver.write[JValue, BsonValue](TypeTagged(value, valueTypeTag.tpe)) match {
      case WriteSuccess(doc)                               => ops.serialize(doc, this)
      // $COVERAGE-OFF$Don't know how to trigger this
      case WriteFailure(f) if f == Seq(WriteError.Nothing) => null // throw WriteException here???
      // $COVERAGE-ON$
    }
  }

  override protected def bakeContext(): Context = {
    val ctx = super.bakeContext()
    ctx.copy(factories = MongoCaseClassTypeAdapter :: BsonObjectIdTypeAdapter :: ctx.factories)
  }
}
