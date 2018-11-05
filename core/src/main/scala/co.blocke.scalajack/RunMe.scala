package co.blocke.scalajack

import co.blocke.scalajack.json.Json4sOps

object MyTypes {
  type Phone = String
}
import MyTypes._

class PhoneSerializer()(implicit tt: TypeTag[Phone]) extends Serializer[Phone] {
  def serialize[AST, S](tagged: TypeTagged[Phone])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
    tagged match {
      case TypeTagged(null) => SerializationSuccess(AstNull())
      case TypeTagged(value) =>
        val fixed = "%s-%s-%s".format(value.substring(0, 3), value.substring(3, 6), value.substring(6))
        SerializationSuccess(AstString(fixed))
    }
}

class PhoneDeserializer()(implicit tt: TypeTag[Phone]) extends Deserializer[Phone] {

  private val nullTypeTagged: TypeTagged[Phone] = TypeTagged[Phone](null.asInstanceOf[Phone], tt.tpe)

  override def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Phone] =
    ast match {
      case AstNull() => DeserializationSuccess(nullTypeTagged)
      case AstString(s) =>
        val fixed: Phone = s.replaceAll("-", "")
        DeserializationSuccess(TypeTagged(fixed, tt.tpe))
    }
}

object PhoneAdapter extends TypeAdapter.=:=[Phone] {
  override val deserializer: Deserializer[Phone] = new PhoneDeserializer()
  override val serializer: Serializer[Phone] = new PhoneSerializer()
}

case class One(p: Phone, s: String)

object RunMe extends App {

  val sj = ScalaJack().withAdapters(PhoneAdapter)

  val o = One("1234567890", "123-456-7890")
  val js = sj.render(o)
  val hey = sj.read[One](js)
  println(js)
  println(hey)
}

