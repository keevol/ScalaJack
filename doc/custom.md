## Custom Type Adapters

ScalaJack does  a great job of reading and rendering data types, but sometimes you just need something custom.  Let's use an example.  Imagine you have a phone number of type String that you want formatted like a US phone number (XXX-XXX-XXXX) but stored as a simple String (no dashes).

To do this ScalaJack allows you to create a custom type adapter and link it into its own chain of adapters.  Let's walk through the process step-by-step.

#### Step 1: Create a Type

```scala
object MyTypes {
  type Phone = String
}
import MyTypes._
```

In this case we create a Phone type to differentiate Phone, which is a String, from any other String value.

#### Step 2: Create an IRTransceiver
We need something that converts your new type Phone back 'n forth between Scala and some IR/AST type.  Two methods in the IRTransceiver trait accomplish this: read() and write().  Read converts the IR into Scala, and write goes from Scala to the IR.

```scala
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

```
Since String is nullable we handle null.  We know value is really a String, so we can do String things to it--some manipulation to strip out the dashes in this case.

You can see in read() we're matching against IR types and producing ReadSuccess instances wrapping Scala types.  Note the TypeTagged wrappings.  Don't let that worry you.  TypeTagged takes the value and the type, which is thoughtfully provided by Scala in the implicit tt:TypeTag[Phone] parameter, so tt.tpe is usually the right thing here.

#### Step 3: Create the PhoneAdapter

```scala
// Override just Phone
object PhoneAdapter extends TypeAdapter.===[Phone] {
  override val irTransceiver: IRTransceiver[Phone] = new PhoneIRTransceiver()
}
```
You can see we specify our IRTransceiver.  One thing that may not be clear is the Typeadapter.===[Phone] notation.  This matches exactly on Phone type, so ScalaJack doesn't confuse other String values with Phone and try to serialize/deserialize them with your custom code.

If what you want is to treat Phone and all subclasses as Phone (with your custom code), then extend TypeAdapter.=:=[Phone] instead. If we did that in this case, every String would be treated as a Phone, with likely dissastrous results.


#### Step 4 (finally): Hook your PhoneAdapter into ScalaJack's list of type adapters

To use your new type adapter, hook it into ScalaJack:

```scala
val sj = ScalaJack().withAdapters(PhoneAdapter)
```

Now anything you parse with a Phone in it will receive special handling via your custom adapter.

**TIP:** withAdapters() is varargs, so you can pass a chain of adapter: ScalaJack().withAdapters(a,b,c,...)

