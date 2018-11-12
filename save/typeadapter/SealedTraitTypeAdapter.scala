package co.blocke.scalajack
package typeadapter

import scala.util.Try

object SealedTraitTypeAdapter extends TypeAdapterFactory {

  trait Subclass[T] {
    type U <: T
    val subclassType: Type
    val subclassClass: Class[U]
    val typeAdapter: TypeAdapter[U]
    val deserializer: Deserializer[U]
    val serializer: Serializer[U]
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
                override val deserializer: Deserializer[U] = subclassTypeAdapter.deserializer
                override val serializer: Serializer[U] = subclassTypeAdapter.serializer
                override val memberNames: Set[MemberName] = memberNames2.toSet
              }
            }

          if (subclassAttempts.exists(_.isFailure)) {
            // If subclassAttempts is full of Failure, the "subclasses" may be case objects, not case classes.
            // This is a common alternative implementation for Enumerations.
            subclassTypes.headOption match {
              case Some(t) if t.typeSymbol.isModuleClass => // Bake a serializer/deserializer than handles case objects
                new TypeAdapter[T] {
                  override val serializer = new CaseObjectSerializer[T]()
                  override val deserializer = new CaseObjectDeserializer[T](subclassTypes.map(_.typeSymbol.name.toString))
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

                override val deserializer = new SealedTraitDeserializer[T](subclasses.map({ subclass =>
                  new SealedTraitDeserializer.Implementation[T] {
                    override val fieldNames: Set[String] = subclass.memberNames
                    override val deserializer: Deserializer[T] = subclass.deserializer
                  }
                }).toSet)

                override val serializer = new SealedTraitSerializer[T](subclasses.map({ subclass =>
                  new SealedTraitSerializer.Implementation[T] {
                    private val runtimeClass: RuntimeClass = subclass.subclassClass
                    private val serializer: Serializer[T] = subclass.typeAdapter.serializer

                    override def isInstance(tagged: TypeTagged[T]): Boolean =
                      tagged match {
                        case TypeTagged(null) => false
                        case TypeTagged(x)    => runtimeClass.isInstance(x)
                      }

                    override def serialize[AST, S](tagged: TypeTagged[T])(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): SerializationResult[AST] =
                      serializer.serialize(tagged)
                  }
                }).toSet)
              }
            }
          }
      }
    } else {
      next.typeAdapterOf[T]
    }

}