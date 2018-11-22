package co.blocke.scalajack
package mongo

import org.bson.{ BsonDocument, BsonValue }
import org.json4s.JValue
import org.json4s.JsonAST.JValue
import org.mongodb.scala.bson.collection.immutable.Document
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

  def readSafely[T](doc: BsonDocument)(implicit tt: TypeTag[T]): Either[ReadFailure, T] = {
    val irTransceiver = context.typeAdapterOf[T].irTransceiver
    ops.deserialize(doc).mapToReadResult(Path.Root, (dsIR: JValue) => irTransceiver.read(Path.Root, dsIR)) match {
      case ReadSuccess(ir) =>
        irTransceiver.read(Path.Root, ir.get) match {
          case ReadSuccess(s)    => Right(s.get)
          case fail: ReadFailure => Left(fail)
        }
      case f: ReadFailure => Left(f)
    }
  }

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): BsonValue = {
    val irTransceiver = context.typeAdapterOf[T].irTransceiver
    irTransceiver.write[JValue, BsonDocument](TypeTagged(value, valueTypeTag.tpe)) match {
      case WriteSuccess(doc)                               => ops.serialize(doc, this)
      case WriteFailure(f) if f == Seq(WriteError.Nothing) => null // throw WriteException here???
    }
  }

  override def parse(doc: BsonValue): DeserializationResult[JValue] = ops.deserialize(doc)
  override def emit(ir: JValue): BsonValue = ops.serialize(ir, this)

  override def materialize[T](ir: BsonValue)(implicit tt: TypeTag[T]): ReadResult[T] =
    context.typeAdapterOf[T].irTransceiver.read(Path.Root, ir) match {
      case res @ ReadSuccess(_) => res
      case fail: ReadFailure    => fail
    }

  override def dematerialize[T](t: T)(implicit tt: TypeTag[T]): WriteResult[JValue] = {
    context.typeAdapterOf[T].writer.write(TypeTagged(t, typeOf[T]))(BsonOps, guidance) match {
      case WriteSuccess(ast)     => ast
      case fail: WriteFailure[_] => throw new WriteException(fail)
    }
  }
  override protected def bakeContext(): Context = {
    val ctx = super.bakeContext()
    ctx.copy(factories = MongoCaseClassTypeAdapter :: BsonDateTimeTypeAdapter :: MongoOffsetDateTimeTypeAdapter :: MongoZonedDateTimeTypeAdapter :: BsonObjectIdTypeAdapter :: ctx.factories)
  }

}
