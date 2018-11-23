package co.blocke.scalajack
package typeadapter

import java.lang.reflect.Method

import co.blocke.scalajack.typeadapter.TupleTypeAdapter.Field

import scala.collection.{ immutable, mutable }
import scala.reflect.runtime.universe.TermName

object TupleTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  val tupleTypeConstructorsByArity: Map[Int, Type] = Map(
    2 -> typeOf[(_, _)].typeConstructor,
    3 -> typeOf[(_, _, _)].typeConstructor,
    4 -> typeOf[(_, _, _, _)].typeConstructor,
    5 -> typeOf[(_, _, _, _, _)].typeConstructor,
    6 -> typeOf[(_, _, _, _, _, _)].typeConstructor,
    7 -> typeOf[(_, _, _, _, _, _, _)].typeConstructor,
    8 -> typeOf[(_, _, _, _, _, _, _, _)].typeConstructor,
    9 -> typeOf[(_, _, _, _, _, _, _, _, _)].typeConstructor,
    10 -> typeOf[(_, _, _, _, _, _, _, _, _, _)].typeConstructor,
    11 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    12 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    13 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    14 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    15 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    16 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    17 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    18 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    19 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    20 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    21 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor,
    22 -> typeOf[(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)].typeConstructor)

  trait Field[Owner] {

    type Value

    val index: Int
    val valueType: Type
    val valueTypeAdapter: TypeAdapter[Value]
    val valueAccessorMethodSymbol: MethodSymbol
    val valueAccessorMethod: Method

    def valueTransceiver: IRTransceiver[Value] = valueTypeAdapter.irTransceiver

    def valueIn(taggedTuple: TypeTagged[Owner]): TypeTagged[Value] =
      taggedTuple match {
        case TypeTagged(tuple) =>
          TypeTagged[Value](valueAccessorMethod.invoke(tuple).asInstanceOf[Value], valueType)
      }
  }

  val tupleFullName = """scala.Tuple(\d+)""".r

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    classSymbol.fullName match {
      case tupleFullName(numberOfFieldsAsString) =>
        val numberOfFields = numberOfFieldsAsString.toInt
        val fieldTypes = tt.tpe.dealias.typeArgs

        val fields = for (i <- 0 until numberOfFields) yield {
          val fieldType = fieldTypes(i)
          val fieldTypeAdapter = context.typeAdapter(fieldType) match {
            case vta: OptionTypeAdapter[_] => vta.noneAsNull
            case vta                       => vta
          }
          val fieldValueAccessorMethodSymbol = tt.tpe.member(TermName(s"_${i + 1}")).asMethod
          val fieldValueAccessorMethod = Reflection.methodToJava(fieldValueAccessorMethodSymbol)

          new Field[T] {
            override type Value = Any
            override val index: Int = i
            override val valueType: Type = fieldType
            override val valueTypeAdapter: TypeAdapter[Value] = fieldTypeAdapter.asInstanceOf[TypeAdapter[Value]]
            override val valueAccessorMethodSymbol: MethodSymbol = fieldValueAccessorMethodSymbol
            override val valueAccessorMethod: Method = fieldValueAccessorMethod
          }
        }

        val classMirror = reflectClass(classSymbol)
        val constructorMirror = classMirror.reflectConstructor(classSymbol.primaryConstructor.asMethod)

        TupleTypeAdapter[T](new TupleIRTransceiver[T](fields, constructorMirror), fields.toList, constructorMirror).asInstanceOf[TypeAdapter[T]]

      case _ =>
        next.typeAdapterOf[T]
    }

}

case class TupleTypeAdapter[T](
    override val irTransceiver: IRTransceiver[T],
    fields:                     List[Field[T]],
    constructorMirror:          MethodMirror) extends TypeAdapter[T]

class TupleIRTransceiver[Tuple](fields: IndexedSeq[Field[Tuple]], tupleConstructorMirror: MethodMirror)(implicit tt: TypeTag[Tuple]) extends IRTransceiver[Tuple] {

  self =>

  private val tupleTypeConstructor: Type = tt.tpe.typeConstructor
  private val nullTypeTagged: TypeTagged[Tuple] = TypeTagged[Tuple](null.asInstanceOf[Tuple], tt.tpe)

  private class TaggedTuple(override val get: Tuple, taggedElements: Array[TypeTagged[Any]]) extends TypeTagged[Tuple] {
    override lazy val tpe: Type = appliedType(tupleTypeConstructor, taggedElements.map(_.tpe).toList)
  }

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[Tuple] =
    ir match {
      case IRArray(elements) =>
        val readResults = new Array[ReadResult[Any]](fields.length)

        val tupleSize = tt.tpe.typeArgs.size
        elements.zipWithIndex.foreach {
          case (element, index) =>
            if (index == tupleSize)
              return ReadFailure(path, ReadError.Unexpected(s"Given JSON has too many elements for tuple", reportedBy = self))
            readResults(index) = fields(index).valueTransceiver.read(path \ index, element)
        }

        if (readResults.exists(_.isFailure)) {
          ReadFailure(readResults.flatMap(_.errors).to[immutable.Seq])
        } else {
          ReadResult(path)({
            val tuple = tupleConstructorMirror(readResults.map(_.get): _*).asInstanceOf[Tuple]
            val taggedElements = readResults.map(_.tagged)
            new TaggedTuple(tuple, taggedElements)
          })
        }
      case IRString(s) if (guidance.isMapKey) => this.read(path, ops.deserialize(path, s.asInstanceOf[WIRE]).get)(ops, guidance = guidance.copy(isMapKey = false))
      case IRNull()                           => ReadSuccess(nullTypeTagged)
      case _                                  => ReadFailure(path, ReadError.Unexpected(s"Expected a JSON array, not $ir", reportedBy = self))
    }

  override def write[IR, WIRE](taggedTuple: TypeTagged[Tuple])(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): WriteResult[IR] =
    taggedTuple match {
      case TypeTagged(null) =>
        WriteSuccess(IRNull())

      case TypeTagged(_) =>
        val errorsBuilder = immutable.Seq.newBuilder[WriteError]

        val telems = mutable.ListBuffer.empty[IR]
        for (field <- fields) {
          val taggedFieldValue = field.valueIn(taggedTuple)
          val fieldSerializationResult = field.valueTransceiver.write(taggedFieldValue)
          fieldSerializationResult match {
            case WriteSuccess(fieldValueIR) => telems += fieldValueIR
            case WriteFailure(fieldErrors) =>
              if (fieldErrors.head.toString == "Nothing")
                telems += ops.applyNull
              else
                errorsBuilder ++= fieldErrors
          }
        }

        val errors = errorsBuilder.result()
        if (errors.nonEmpty) {
          WriteFailure(errors)
        } else {
          WriteSuccess(IRArray(telems))
        }
    }
}
