package co.blocke.scalajack
package typeadapter

import scala.collection.GenMap
import scala.collection.mutable
import scala.collection.generic.CanBuildFrom

object MapTypeAdapter extends TypeAdapterFactory.<:<.withTwoTypeParams[GenMap] {

  override def create[K, V, M <: GenMap[K, V]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[M], ttMap: TypeTag[GenMap[K, V]], ttKey: TypeTag[K], ttValue: TypeTag[V]): TypeAdapter[M] = {
    val keyTypeAdapter = context.typeAdapterOf[K]
    val valueTypeAdapter = context.typeAdapterOf[V]

    CanBuildFroms.to[M].headOption match {
      case None =>
        next.typeAdapterOf[M]

      case Some(canBuildFromEntry) =>
        val canBuildFrom = canBuildFromEntry.canBuildFrom.asInstanceOf[CanBuildFrom[_, (K, V), M]]

        def newBuilder(): mutable.Builder[(K, V), M] = canBuildFrom()

        val mapIRTransceiver = new MapIRTransceiver[K, V, M](
          keyTransceiver          = keyTypeAdapter.irTransceiver,
          valueTransceiver        = valueTypeAdapter.irTransceiver,
          keyValuePairTransceiver = context.typeAdapterOf[List[Tuple2[K, V]]].irTransceiver,
          () => newBuilder)

        MapTypeAdapter[K, V, M](mapIRTransceiver)
    }
  }

}

case class MapTypeAdapter[K, V, M <: GenMap[K, V]](override val irTransceiver: IRTransceiver[M]) extends TypeAdapter[M]

object CanBuildFroms {

  //  def main(args: Array[String]): Unit = {
  //    val entries = CanBuildFroms.to[Map[_, _]]
  //
  //    for (entry <- entries) {
  //      println(entry.canBuildFrom.apply().result())
  //    }
  //
  //    println(entries)
  //  }

  trait Entry[To] {
    type From
    type Elem

    implicit val ttFrom: TypeTag[From]
    implicit val ttElem: TypeTag[Elem]
    val canBuildFrom: CanBuildFrom[From, Elem, To]
  }

  def to[To](implicit tt: TypeTag[To]): Seq[Entry[To]] = {
    val classSymbol: ClassSymbol = tt.tpe.typeSymbol.asClass

    val companionSymbol = classSymbol.companion.asModule
    val companionType = companionSymbol.info

    // Examples in comments reference Scala's List[A] type.

    val methods = for (member <- companionType.members if member.isMethod) yield member.asMethod

    // `implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]] = ...`
    val implicitConversions = for (method <- methods if method.isImplicit && method.paramLists.flatten.isEmpty && method.returnType <:< typeOf[CanBuildFrom[_, _, _]]) yield method

    val entries: Seq[Entry[To]] = implicitConversions.toSeq map { method =>
      // returnTypeAsCanBuildFrom == CanBuildFrom[Coll, A, List[A]]
      val returnTypeAsCanBuildFrom = method.returnType.baseType(typeOf[CanBuildFrom[_, _, _]].typeSymbol)

      // typeParam == A
      val typeParams = method.typeParams

      // toType == List[A]
      val toType = returnTypeAsCanBuildFrom.typeArgs(2)

      val typeParamSubstitutions: List[(Symbol, Type)] = typeParams flatMap { typeParam =>
        // typeParam == A
        // optionalTypeArg == Some(String)
        val optionalTypeArg = Reflection.solveForNeedleAfterSubstitution(
          haystackBeforeSubstitution = toType,
          haystackAfterSubstitution  = tt.tpe.baseType(toType.typeSymbol),
          needleBeforeSubstitution   = typeParam.asType.toType)
        optionalTypeArg.map(typeArg => typeParam -> typeArg)
      }

      // elementTypeBeforeSubstitution == A
      val elementTypeBeforeSubstitution = returnTypeAsCanBuildFrom.typeArgs(1)
      // elementTypeAfterSubstitution == String
      val elementTypeAfterSubstitution = elementTypeBeforeSubstitution.substituteTypes(typeParamSubstitutions.map(_._1), typeParamSubstitutions.map(_._2))
      val companionInstance = reflectModule(companionSymbol).instance
      val canBuildFrom_ = reflect(companionInstance).reflectMethod(method).apply().asInstanceOf[CanBuildFrom[Any, Any, To]]

      new Entry[To] {
        override type From = Any
        override type Elem = Any
        override implicit val ttFrom: TypeTag[From] = TypeTags.of[From](returnTypeAsCanBuildFrom.typeArgs.head)
        override implicit val ttElem: TypeTag[Elem] = TypeTags.of[Elem](elementTypeAfterSubstitution)
        override val canBuildFrom: CanBuildFrom[From, Elem, To] = canBuildFrom_.asInstanceOf[CanBuildFrom[From, Elem, To]]
      }
    }
    entries
  }
}
