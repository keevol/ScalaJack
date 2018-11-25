package co.blocke.scalajack
package dynamodb
package test

object MyTypes {
  type Phone = String
}
import MyTypes._

class PhoneIRTransceiver()(implicit tt: TypeTag[Phone]) extends IRTransceiver[Phone] {

  private val nullTypeTagged: TypeTagged[Phone] = TypeTagged[Phone](null.asInstanceOf[Phone], tt.tpe)

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Phone] =
    ir match {
      case IRNull() => ReadSuccess(nullTypeTagged)
      case IRString(s) =>
        val fixed: Phone = s.replaceAll("-", "")
        ReadSuccess(TypeTagged(fixed, tt.tpe))
    }

  override def write[IR, WIRE](tagged: TypeTagged[Phone])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(null) => WriteSuccess(IRNull())
      case TypeTagged(value) =>
        val fixed = "%s-%s-%s".format(value.substring(0, 3), value.substring(3, 6), value.substring(6))
        WriteSuccess(IRString(fixed))
    }
}

// Override just Phone
object PhoneAdapter extends TypeAdapter.===[Phone] {
  override val irTransceiver: IRTransceiver[Phone] = new PhoneIRTransceiver()
}
trait Human { val name: String; val age: Int }
case class Misc(wow: Double, bing: String)

@Collection(name = "people")
case class Person(
    @DBKey(index = 1) name:String,
    @DBKey(index = 0) age:Int,
    likes:                List[String],
    stuff:                Misc,
    foo:                  Option[Boolean] = None) extends Human

@Collection(name = "people2")
case class PersonOneKey(
    @DBKey(index = 0) name:String,
    age:                  Int,
    likes:                List[String],
    stuff:                Misc,
    foo:                  Option[Boolean] = None) extends Human

@Collection(name = "bogus")
case class ErrorNoKey(
    name:  String,
    age:   Int,
    likes: List[String],
    stuff: Misc,
    foo:   Option[Boolean] = None) extends Human

case class ErrorNoTable(
    @DBKey(index = 0) name:String,
    age:                  Int,
    likes:                List[String],
    stuff:                Misc,
    foo:                  Option[Boolean] = None) extends Human

case class PersonWithPhone(name: String, phone: Phone)
trait Address { val postalCode: String }
case class DefaultAddress(postalCode: String) extends Address
case class USAddress(street: String, city: String, state: String, postalCode: String) extends Address

@Collection(name = "people")
class PersonPlain1(
    @DBKey(index = 1) val name:String,
    @DBKey(index = 0) val age:Int,
    val likes:                List[String],
    val stuff:                Misc,
    val foo:                  Option[Boolean] = None)

@Collection(name = "people")
class PersonPlain2() {
  @DBKey(index = 1) var name: String = ""
  @DBKey(index = 0) var age: Int = 0
  var likes: List[String] = List.empty[String]
  var stuff: Misc = null
  var foo: Option[Boolean] = None
}

trait Body
case class FancyBody(message: String) extends Body

case class Envelope[T <: Body](id: String, body: T) {
  type Giraffe = T
}
