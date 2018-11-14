package co.blocke.scalajack
package typeadapter

import scala.collection.immutable
import scala.util.Try

object SealedTraitTypeAdapter extends TypeAdapterFactory {

  trait Subclass[T] {
    type U <: T
    val subclassType: Type
    val subclassClass: Class[U]
    val typeAdapter: TypeAdapter[U]
    val irTransceiver: IRTransceiver[U]
    val memberNames: Set[MemberName]
  }

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe.typeSymbol.isClass) {
      tt.tpe.typeSymbol.asClass.knownDirectSubclasses.toList match {
        case Nil =>
          next.typeAdapterOf[T]

        case subclassSymbols =>
          val subclassTypes = subclassSymbols.map(_.asType.toType)

          val subclassAttempts =
            for (subclassType2 <- subclassTypes) yield Try {
              type U = T

              val subclassTypeAdapter = context.typeAdapter(subclassType2).asInstanceOf[TypeAdapter[U]]
              val memberNames2 = subclassTypeAdapter.as[ClassLikeTypeAdapter[Any]].members.map(_.name)
              new Subclass[T] {
                override type U = T
                override val subclassType: Type = subclassType2
                override val subclassClass: Class[U] = runtimeClass(subclassType).asInstanceOf[Class[U]]
                override val typeAdapter: TypeAdapter[U] = subclassTypeAdapter
                override val irTransceiver: IRTransceiver[U] = subclassTypeAdapter.irTransceiver
                override val memberNames: Set[MemberName] = memberNames2.toSet
              }
            }

          if (subclassAttempts.exists(_.isFailure)) {
            // If subclassAttempts is full of Failure, the "subclasses" may be case objects, not case classes.
            // This is a common alternative implementation for Enumerations.
            subclassTypes.headOption match {
              case Some(t) if t.typeSymbol.isModuleClass => // Bake a serializer/deserializer than handles case objects
                new TypeAdapter[T] {
                  override val irTransceiver = new CaseObjectIRTransceiver[T](subclassTypes.map(_.typeSymbol.name.toString))
                }
              case _ => typeAdapterOf[T]
            }
          } else {
            val subclasses = subclassAttempts.map(_.get)

            def allPairsOf[E](superset: Set[E]): Set[(E, E)] =
              superset.subsets(2)
                .map(_.toList)
                .map(list => {
                  val a :: b :: Nil = list
                  (a, b)
                })
                .toSet

            val allPairsOfSubclasses: Set[(Subclass[T], Subclass[T])] = allPairsOf(subclasses.toSet)

            val someSubclassesAreAmbiguous = allPairsOfSubclasses.exists({
              case (a, b) =>
                a.memberNames.subsetOf(b.memberNames) || b.memberNames.subsetOf(a.memberNames)
            })

            if (someSubclassesAreAmbiguous) {
              next.typeAdapterOf[T]
            } else {
              new TypeAdapter[T] {
                override val irTransceiver = new SealedTraitIRTransceiver[T](subclasses.map({ subclass =>
                  new SealedTraitIRTransceiver.Implementation[T] {
                    private val runtimeClass: RuntimeClass = subclass.subclassClass
                    override val fieldNames: Set[String] = subclass.memberNames
                    override val irTransceiver = subclass.irTransceiver
                    override def isInstance(tagged: TypeTagged[T]): Boolean =
                      tagged match {
                        case TypeTagged(null) => false
                        case TypeTagged(x)    => runtimeClass.isInstance(x)
                      }
                    override def write[IR](tagged: TypeTagged[T])(implicit ops: OpsBase[IR], guidance: SerializationGuidance): WriteResult[IR] =
                      irTransceiver.write(tagged)
                  }
                }).toSet)

                //                override val serializer = new SealedTraitSerializer[T](subclasses.map({ subclass =>
                //                  new SealedTraitIRTransceiver.Implementation[T] {
                //                    private val runtimeClass: RuntimeClass = subclass.subclassClass
                //                    private val serializer: Serializer[T] = subclass.typeAdapter.serializer
                //
                //                    override def serialize[AST, S](tagged: TypeTagged[T])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
                //                      serializer.serialize(tagged)
                //                  }
                //                }).toSet)
              }
            }
          }
      }
    } else {
      next.typeAdapterOf[T]
    }
}

class CaseObjectIRTransceiver[T](subclasses: List[String])(implicit tt: TypeTag[T]) extends IRTransceiver[T] {

  self =>

  private val CaseObjectType: Type = typeOf[T]
  private val taggedNull: TypeTagged[T] = TypeTagged(null.asInstanceOf[T], tt.tpe)

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[T] =
    ir match {
      case IRNull() => ReadSuccess(taggedNull)

      case IRString(s) if (subclasses.contains(s)) =>
        val clazz = Class.forName(tt.tpe.typeSymbol.asClass.owner.fullName + "." + s + "$")
        val objInstance = clazz.getField("MODULE$").get(null).asInstanceOf[T]
        ReadSuccess(TypeTagged(objInstance, typeOf[T]))

      case _ => ReadFailure(path, ReadError.Unexpected(s"Expected a valid subclass of $CaseObjectType", reportedBy = self))
    }

  override def write[IR](tagged: TypeTagged[T])(implicit ops: OpsBase[IR], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(null) => WriteSuccess(IRNull())
      case TypeTagged(c)    => WriteSuccess(IRString(c.toString))
    }
}

object SealedTraitIRTransceiver {
  trait Implementation[T] {
    val fieldNames: immutable.Set[String]
    val irTransceiver: IRTransceiver[T]
    def isInstance(tagged: TypeTagged[T]): Boolean
    def write[IR](tagged: TypeTagged[T])(implicit ops: OpsBase[IR], guidance: SerializationGuidance): WriteResult[IR]
  }
}

class SealedTraitIRTransceiver[T](implementations: immutable.Set[SealedTraitIRTransceiver.Implementation[T]])(implicit tt: TypeTag[T]) extends IRTransceiver[T] {

  self =>

  override def read[IR, WIRE](path: Path, ir: IR)(implicit ops: Ops[IR, WIRE], guidance: SerializationGuidance): ReadResult[T] =
    ir match {
      case IRObject(fields) =>
        val allFieldNames = fields.map(_._1).toSet
        implementations.filter(implementation => implementation.fieldNames.subsetOf(allFieldNames)) match {
          case emptySet if emptySet.isEmpty =>
            throw new RuntimeException(s"No sub-classes of ${tt.tpe.typeSymbol.fullName} match field names $allFieldNames")

          case setOfOne if setOfOne.size == 1 =>
            val implementation = setOfOne.head
            implementation.irTransceiver.read(path, ir)

          case _ =>
            throw new RuntimeException(s"Multiple sub-classes of ${tt.tpe.typeSymbol.fullName} match field names $allFieldNames")
        }

      case _ =>
        ReadFailure(path, ReadError.Unexpected("Expected a JSON object", reportedBy = self))
    }

  override def write[IR](tagged: TypeTagged[T])(implicit ops: OpsBase[IR], guidance: SerializationGuidance): WriteResult[IR] =
    tagged match {
      case TypeTagged(null) => WriteSuccess(IRNull())
      case TypeTagged(_) =>
        implementations.find(_.isInstance(tagged)) match {
          case Some(implementation) => implementation.write(tagged)
          case None                 => ??? // TODO: What's this ??? mean here?
        }
    }
}
