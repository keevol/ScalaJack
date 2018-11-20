package co.blocke.scalajack
package json

import org.json4s.JsonAST.{ JArray, JBool, JDecimal, JDouble, JInt, JLong, JNull, JObject, JString, JValue }

trait Json4sOpsBase extends OpsBase[JValue] {

  override type ArrayType = JArray
  override type ObjectType = JObject

  override def getArrayElement(arr: JArray, index: Int): Option[JValue] =
    if (index >= 0 && index < arr.values.size) Some(arr(index)) else None

  override def applyArray(values: Seq[JValue]): JValue = new JArray(values.toList)
  override def unapplyArray(ir: JValue): Option[Seq[JValue]] =
    ir match {
      case elements: JArray => Some(elements.children)
      case _                => None
    }

  override def applyBoolean(value: Boolean): JValue = JBool(value)
  override def unapplyBoolean(ir: JValue): Option[Boolean] =
    ir match {
      case JBool(value) => Some(value)
      case _            => None
    }

  override def applyDecimal(value: BigDecimal): JValue = JDecimal(value)
  override def unapplyDecimal(ir: JValue): Option[BigDecimal] =
    ir match {
      case JDecimal(value) => Some(value)
      case _               => None
    }

  override def applyDouble(value: Double): JValue = JDouble(value)
  override def unapplyDouble(ir: JValue): Option[Double] =
    ir match {
      case JDouble(value) => Some(value)
      case _              => None
    }

  override def applyInt(value: BigInt): JValue = JInt(value)
  override def unapplyInt(ir: JValue): Option[BigInt] =
    ir match {
      case JInt(value) => Some(value)
      case _           => None
    }

  override def applyLong(value: Long): JValue = JLong(value)
  override def unapplyLong(ir: JValue): Option[Long] =
    ir match {
      case JLong(value) => Some(value)
      case _            => None
    }

  override def applyNull(): JValue = JNull
  override def unapplyNull(ir: JValue): Boolean =
    ir match {
      case JNull => true
      case _     => false
    }

  override def applyObject(elements: Seq[(String, JValue)]): JValue = new JObject(elements.toList)
  override def unapplyObject(ir: JValue): Option[Seq[(String, JValue)]] =
    ir match {
      case JObject(elements) => Some(elements)
      case _                 => None
    }

  override def applyString(string: String): JValue = JString(string)
  override def unapplyString(ir: JValue): Option[String] =
    ir match {
      case JString(value) => Some(value)
      case _              => None
    }
}

object Json4sOps extends Ops[JValue, String] with Json4sOpsBase with JsonDeserializer[JValue] with JsonSerializer[JValue]
