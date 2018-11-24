package co.blocke.scalajack
package csv

import scala.reflect.runtime.universe.Type
import java.lang.{ UnsupportedOperationException => UOE }

import typeadapter.{ CaseClassTypeAdapter, CanBuildFromTypeAdapter }
import org.json4s.JsonAST.JValue

case class CSVFlavor() extends {
  val customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory]
  val hintMap: Map[Type, String] = Map.empty[Type, String]
  val hintModifiers: Map[Type, HintModifier] = Map.empty[Type, HintModifier]
  val typeModifier: Option[HintModifier] = None
  val parseOrElseMap: Map[Type, Type] = Map.empty[Type, Type]
  val defaultHint: String = "_hint"
  val isCanonical: Boolean = true
  val secondLookParsing: Boolean = false
} with ScalaJackLike[JValue, String] {

  def withAdapters(ta: TypeAdapterFactory*) = throw new UOE("Not available for CSV formatting")
  def withHints(h: (Type, String)*) = throw new UOE("Not available for CSV formatting")
  def withHintModifiers(hm: (Type, HintModifier)*) = throw new UOE("Not available for CSV formatting")
  def withDefaultHint(hint: String) = throw new UOE("Not available for CSV formatting")
  def withTypeModifier(tm: HintModifier) = throw new UOE("Not available for CSV formatting")
  def withSecondLookParsing() = throw new UOE("Not available for CSV formatting")
  def isCanonical(canonical: Boolean) = throw new UOE("Not available for CSV formatting")
  def parseOrElse(poe: (Type, Type)*) = throw new UOE("Not available for CSV formatting")

  implicit val ops = CSVOps

  // We use withMapValue here to force handling of Optional class fields for CSV.  For Json, optional fields are just dropped/ignored,
  // but for csv we need to emit an empty field.  This flag forces CSV-correct handling within OptionSerializer
  implicit val guidance: SerializationGuidance = SerializationGuidance().withMapValue()

  // Reading CSV is wonky.  We need fieldname/value pairs and CVS only gives us ordered values, sooo....
  // We need to grab the internals of the case class, specifically the fields, then (in order) create the
  // fieldname->value map required by the case class irTransceiver.  Additionally there may be some type slip 'n slide
  // as CSV is free-form, so we need to do some smart-matching on the AST types against the required field types.
  def readSafely[T](csv: String)(implicit tt: TypeTag[T]): Either[ReadFailure, T] = {
    val typeAdapter = context.typeAdapterOf[T]

    ops.deserialize(Path.Root, csv).get match {
      case ir @ IRArray(elements) =>
        // T could validly be either an Array or an Object... but which?
        typeAdapter.maybeAs[CaseClassTypeAdapter[T]] match {
          case Some(ccta) =>
            val fields = ccta.members.map(member => (member.name, member.asInstanceOf[CaseClassTypeAdapter.FieldMember[_, _]].valueType))
            val objIR = IRObject(elements.zipWithIndex.map {
              case (value, pos) =>
                (fields(pos)._1, fixedValue(value, fields(pos)._2))
            })
            ccta.irTransceiver.read(Path.Root, objIR) match {
              case ReadSuccess(t)       => Right(t.get)
              case failure: ReadFailure => Left(failure)
            }
          case None => // Array?
            typeAdapter.maybeAs[CanBuildFromTypeAdapter[T, Seq[T]]] match {
              case Some(arta) =>
                arta.irTransceiver.read(Path.Root, ir) match {
                  case ReadSuccess(t)       => Right(t.get.asInstanceOf[T])
                  // $COVERAGE-OFF$WriteFailure not fully socialized
                  case failure: ReadFailure => Left(failure)
                  // $COVERAGE-ON$            }
                }
              // $COVERAGE-OFF$Don't know how to trigger this
              case None => Left(ReadFailure(Path.Root, ReadError.Unexpected("Unable to successfully deserialize this CSV", typeAdapter.irTransceiver)))
              // $COVERAGE-ON$
            }
        }

      case IRNull() => Right(null.asInstanceOf[T])
      case _ =>
        // $COVERAGE-OFF$Should never happen(tm).  Means we had something in CSV that wasn't Null or an Array, which isn't possible in this model.
        Left(ReadFailure(Path.Root, ReadError.Unexpected("Unable to successfully deserialize this CSV", typeAdapter.irTransceiver)))
      // $COVERAGE-ON$
    }
  }

  // Fuzzy "safe" conversions to handle the fact that CSV is pretty free-from
  private def fixedValue(irValue: JValue, ccFieldType: Type): JValue =
    irValue match {
      case IRDouble(d) if typeOf[String] == ccFieldType => IRString(d.toString)
      case IRLong(n) if typeOf[String] == ccFieldType => IRString(n.toString)
      case IRBoolean(b) if typeOf[String] == ccFieldType => IRString(b.toString)
      case _ => irValue
    }

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): String = {
    val typeAdapter = context.typeAdapterOf[T]
    val serializer = typeAdapter.irTransceiver
    serializer.write[JValue, String](TypeTagged(value, valueTypeTag.tpe)) match {
      case WriteSuccess(objectOutput) =>
        ops.serialize(objectOutput, this)
      case WriteFailure(f) if f == Seq(WriteError.Nothing) => ""
    }
  }

  override def parse(wire: String): DeserializationResult[JValue] = ops.deserialize(Path.Root, wire)
  override def emit(ir: JValue): String = ops.serialize(ir, this)

  override def materialize[T](ir: JValue)(implicit tt: TypeTag[T]): ReadResult[T] =
    context.typeAdapterOf[T].irTransceiver.read(Path.Root, ir) match {
      case res @ ReadSuccess(_) => res
      case fail: ReadFailure    => fail
    }

  override def dematerialize[T](t: T)(implicit tt: TypeTag[T]): WriteResult[JValue] =
    context.typeAdapterOf[T].irTransceiver.write(TypeTagged(t, typeOf[T])) match {
      case res @ WriteSuccess(_) => res
      // $COVERAGE-OFF$WriteFailure not fully socialized
      case fail: WriteFailure    => fail
      // $COVERAGE-ON$
    }
}
