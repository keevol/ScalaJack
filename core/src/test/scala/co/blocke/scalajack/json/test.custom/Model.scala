package co.blocke.scalajack
package json.test.custom

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

// Override Phone...and its parents (String)!
object OopsPhoneAdapter extends TypeAdapter.=:=[Phone] {
  override val irTransceiver: IRTransceiver[Phone] = new PhoneIRTransceiver()
}

case class Person(name: String, phone: Phone)

trait Address { val postalCode: String }
case class USAddress(street: String, city: String, state: String, postalCode: String) extends Address
case class CanadaAddress(street: String, city: String, province: String, postalCode: String) extends Address
case class DefaultAddress(postalCode: String) extends Address
trait Demographic { val address: Address }
case class USDemographic(age: Int, address: Address) extends Demographic
