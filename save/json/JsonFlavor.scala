package co.blocke.scalajack
package json

import org.json4s.JsonAST.{ JNull, JValue }

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
  implicit val ops: AstOps[JValue, String] = Json4sOps.asInstanceOf[AstOps[JValue, String]]

  def readSafely[T](json: String)(implicit tt: TypeTag[T]): Either[DeserializationFailure, T] = {
    val deserializationResult = try {
      val Some(js) = Json4sOps.parser._parse(json)
      val deserializer = context.typeAdapterOf[T].deserializer
      deserializer.deserialize(Path.Root, js)
    } catch {
      case e: Exception =>
        DeserializationFailure(Path.Unknown, DeserializationError.ExceptionThrown(e))
    }
    deserializationResult match {
      case DeserializationSuccess(TypeTagged(result)) =>
        Right(result)

      case failure @ DeserializationFailure(_) =>
        Left(failure)
    }
  }

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): String = {
    val typeAdapter = context.typeAdapterOf[T]
    val serializer = typeAdapter.serializer
    serializer.serialize[JValue, String](TypeTagged(value, valueTypeTag.tpe)) match {
      case SerializationSuccess(json)                                      => Json4sOps.renderCompact(json, this)
      case SerializationFailure(f) if f == Seq(SerializationError.Nothing) => ""
    }
  }

  def parseToAST(json: String): JValue =
    Json4sOps.parser._parse(json)(Json4sOps).getOrElse(JNull)

  def emitFromAST(ast: JValue): String =
    Json4sOps.renderCompact(ast, this)

  def materialize[T](ast: JValue)(implicit tt: TypeTag[T]): T =
    context.typeAdapterOf[T].deserializer.deserialize(Path.Root, ast) match {
      case DeserializationSuccess(ok)   => ok.get
      case fail: DeserializationFailure => throw new DeserializationException(fail)
    }

  def dematerialize[T](t: T)(implicit tt: TypeTag[T]): JValue = {
    context.typeAdapterOf[T].serializer.serialize(TypeTagged(t, typeOf[T])) match {
      case SerializationSuccess(ast)     => ast
      case fail: SerializationFailure[_] => throw new SerializationException(fail)
    }
  }
}