package co.blocke.scalajack
package json

import org.json4s.JsonAST.JValue

import scala.reflect.runtime.universe.Type

case class JsonFlavor(
    customAdapters:    List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    hintMap:           Map[Type, String]        = Map.empty[Type, String],
    hintModifiers:     Map[Type, HintModifier]  = Map.empty[Type, HintModifier],
    typeModifier:      Option[HintModifier]     = None,
    parseOrElseMap:    Map[Type, Type]          = Map.empty[Type, Type],
    defaultHint:       String                   = "_hint",
    isCanonical:       Boolean                  = true,
    secondLookParsing: Boolean                  = false) extends ScalaJackLike[JValue, String] {

  def withAdapters(ta: TypeAdapterFactory*) = this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withHints(h: (Type, String)*) = this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintModifier)*) = this.copy(hintModifiers = this.hintModifiers ++ hm)
  def withDefaultHint(hint: String) = this.copy(defaultHint = hint)
  def withTypeModifier(tm: HintModifier) = this.copy(typeModifier = Some(tm))
  def parseOrElse(poe: (Type, Type)*) = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def isCanonical(canonical: Boolean) = this.copy(isCanonical = canonical)
  def withSecondLookParsing() = this.copy(secondLookParsing = true)

  implicit val guidance: SerializationGuidance = {
    val first = if (secondLookParsing)
      SerializationGuidance(secondLookParsing = true)
    else
      SerializationGuidance()
    if (isCanonical)
      first.copy(isCanonical = true)
    else
      first
  }

  implicit val ops: Ops[JValue, String] = Json4sOps

  def readSafely[T](json: String)(implicit tt: TypeTag[T]): Either[ReadFailure, T] =
    Json4sOps.deserialize(json) match {
      case DeserializationFailure(df) =>
        Left(ReadFailure(Path.Root, df: _*))
      case DeserializationSuccess(ir) =>
        try {
          context.typeAdapterOf[T].irTransceiver.read(Path.Root, ir) match {
            case rf: ReadFailure    => Left(rf)
            case ReadSuccess(scala) => Right(scala.get)
          }
        } catch {
          case t: Throwable => Left(ReadFailure(Path.Root, ReadError.ExceptionThrown(t)))
        }
    }

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): String = {
    val typeAdapter = context.typeAdapterOf[T]
    typeAdapter.irTransceiver.write(TypeTagged(value, valueTypeTag.tpe)) match {
      case WriteSuccess(json)                              => Json4sOps.serialize(json, this)
      case WriteFailure(f) if f == Seq(WriteError.Nothing) => ""
    }
  }

  def parseToAST(json: String): DeserializationResult[JValue] = Json4sOps.deserialize(json)
  def emitFromAST(ir: JValue): String = Json4sOps.serialize(ir, this)
  def materialize[T](ir: JValue)(implicit tt: TypeTag[T]): ReadResult[T] = context.typeAdapterOf[T].irTransceiver.read(Path.Root, ir)
  def dematerialize[T](t: T)(implicit tt: TypeTag[T]): WriteResult[JValue] = context.typeAdapterOf[T].irTransceiver.write(TypeTagged(t, typeOf[T]))
}
