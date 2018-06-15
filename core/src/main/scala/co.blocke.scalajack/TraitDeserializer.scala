package co.blocke.scalajack

import java.util.concurrent.{ ConcurrentHashMap, TimeUnit }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

object TraitDeserializer {

  private val cache = new ConcurrentHashMap[Type, Future[TraitDeserializer[_]]]()

  def apply[T]()(implicit tt: TypeTag[T], context: Context): TraitDeserializer[T] = {

    val deserializerFuture: Future[TraitDeserializer[T]] = cache.computeIfAbsent(tt.tpe, new java.util.function.Function[Type, Future[TraitDeserializer[_]]] {
      override def apply(traitType: Type): Future[TraitDeserializer[_]] =
        Future(generateDeserializer[T]())
    }).asInstanceOf[Future[TraitDeserializer[T]]]

    val deserializer = Await.result(deserializerFuture, FiniteDuration(30, TimeUnit.SECONDS))

    deserializer
  }

  def generateDeserializer[T]()(implicit tt: TypeTag[T], context: Context): TraitDeserializer[T] = {
    val classSymbol: ClassSymbol = tt.tpe.typeSymbol.asClass
    val reflectMembers = classSymbol.typeSignature.members.filter(_.isAbstract).toList

    class FieldMember(val name: String, val valueType: Type) {

      def isPathDependentType: Boolean =
        valueType match {
          case TypeRef(SingleType(ThisType(`classSymbol`), _), _, _) =>
            true
          case _ =>
            false
        }

      def valueTypeAsScala: String =
        valueType match {
          case TypeRef(SingleType(ThisType(`classSymbol`), fieldSymbol), typeMemberOfFieldSymbol, _) =>
            s"${fieldSymbol.name.decodedName}.${typeMemberOfFieldSymbol.name.decodedName}"

          case _ =>
            valueType.toString
        }

    }

    val members: List[FieldMember] = reflectMembers map { reflectMember =>
      new FieldMember(name      = reflectMember.name.decodedName.toString, valueType = reflectMember.asMethod.returnType)
    }

    val deserializerClass =
      ScalaCompiler.compileClass(
        s"""
           |class Deferred[J](override val thisPath: co.blocke.scalajack.Path, override val thisJson: J${members.filterNot(_.isPathDependentType).map(member => s""", ${member.name}Deserializer: co.blocke.scalajack.Deserializer[${member.valueType}]""").mkString("")})(implicit override val thisJsonOps: co.blocke.scalajack.JsonOps[J]) extends ${tt.tpe} with co.blocke.scalajack.DeserializedTrait[J] {
           |
           |  import co.blocke.{ scalajack => sj }
           |
           |${members.filterNot(_.isPathDependentType).map(member => s"""  private lazy val ${member.name}DeserializationResult: sj.DeserializationResult[${member.valueTypeAsScala}] = deserializeField("${member.name}", ${member.name}Deserializer)""").mkString("\n\n")}
           |
           |${members.map(member => s"""  override lazy val ${member.name}: ${member.valueTypeAsScala} = ${if (member.isPathDependentType) "???" else s"""${member.name}DeserializationResult.get.get"""}""").mkString("\n\n")}
           |
           |}
           |
           |class ThisTraitDeserializer(implicit tt: co.blocke.scalajack.TypeTag[${tt.tpe}]${members.filterNot(_.isPathDependentType).map(member => s""", ${member.name}Deserializer: co.blocke.scalajack.Deserializer[${member.valueType}]""").mkString("")}) extends co.blocke.scalajack.TraitDeserializer[${tt.tpe}] {
           |
           |  import co.blocke.{ scalajack => sj }
           |
           |  override def deserialize[J](path: sj.Path, json: J)(implicit ops: sj.JsonOps[J]): sj.DeserializationResult[${tt.tpe}] = {
           |    sj.DeserializationSuccess(sj.TypeTagged(new Deferred[J](path, json${members.filterNot(_.isPathDependentType).map(member => s""", ${member.name}Deserializer""").mkString("")}), tt.tpe))
           |  }
           |
           |}
           |
           |scala.reflect.classTag[ThisTraitDeserializer].runtimeClass
      """.stripMargin)

    val deserializers = members.filterNot(_.isPathDependentType).map(_.valueType).map(context.deserializer)

    val deserializer = deserializerClass.getConstructors()(0).newInstance(List(tt) ++ deserializers: _*).asInstanceOf[TraitDeserializer[T]]
    deserializer
  }

}

trait TraitDeserializer[T] extends Deserializer[T] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T] = ???

}

trait DeserializedTrait[J] {

  val thisPath: Path

  val thisJson: J

  implicit val thisJsonOps: JsonOps[J]

  def deserializeField[V](name: String, valueDeserializer: Deserializer[V]): DeserializationResult[V] =
    thisJson match {
      case JsonObject(x) =>
        val fields = x.asInstanceOf[thisJsonOps.ObjectFields]

        val deserializationResult: DeserializationResult[V] =
          thisJsonOps.getObjectField(fields, name) match {
            case Some(valueJson) =>
              valueDeserializer.deserialize(thisPath \ name, valueJson)

            case None =>
              valueDeserializer.deserializeFromNothing(thisPath \ name)
          }

        deserializationResult
    }

}