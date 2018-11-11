package co.blocke.scalajack
package json

import org.json4s.JsonAST.{ JArray, JBool, JDecimal, JDouble, JInt, JLong, JNothing, JNull, JObject, JString, JValue }

trait Json4sOpsBase extends AstBase[JValue] {

  override type ArrayElements = JArray
  override type ObjectFields = JObject

  override def foreachArrayElement(elements: ArrayElements, f: (Int, JValue) => Unit): Unit = {
    for ((element, index) <- elements.arr.zipWithIndex if element != JNothing) {
      f(index, element)
    }
  }

  override def foreachObjectField(fields: ObjectFields, f: (String, JValue) => Unit): Unit = {
    for ((name, value) <- fields.obj if value != JNothing) {
      f(name, value)
    }
  }

  override def getObjectField(fields: ObjectFields, name: String): Option[JValue] =
    fields.obj.find(_._1 == name).map(_._2).filter(_ != JNothing)
  override def getArrayElement(arr: ArrayElements, index: Int): Option[JValue] =
    if (arr.values.size < index && index >= 0) Some(arr(index)) else None

  override def partitionObjectFields(fields: ObjectFields, fieldNames: List[String]): (ObjectFields, ObjectFields) = {
    val (a, b) = fields.obj.partition(f => fieldNames.contains(f._1))
    (JObject(a), JObject(b))
  }

  override def applyArray(appendAllElements: (JValue => Unit) => Unit): JValue = {
    val elementsBuilder = List.newBuilder[JValue]

    appendAllElements { element =>
      elementsBuilder += element
    }

    JArray(elementsBuilder.result())
  }

  override def unapplyArray(json: JValue): Option[JArray] =
    json match {
      case elements: JArray => Some(elements)
      case _                => None
    }

  override def applyBoolean(value: Boolean): JValue =
    JBool(value)

  override def unapplyBoolean(json: JValue): Option[Boolean] =
    json match {
      case JBool(value) => Some(value)
      case _            => None
    }

  override def applyDecimal(value: BigDecimal): JValue =
    JDecimal(value)

  override def unapplyDecimal(json: JValue): Option[BigDecimal] =
    json match {
      case JDecimal(value) => Some(value)
      case _               => None
    }

  override def applyDouble(value: Double): JValue =
    JDouble(value)

  override def unapplyDouble(json: JValue): Option[Double] =
    json match {
      case JDouble(value) => Some(value)
      case _              => None
    }

  override def applyInt(value: BigInt): JValue =
    JInt(value)

  override def unapplyInt(json: JValue): Option[BigInt] =
    json match {
      case JInt(value) => Some(value)
      case _           => None
    }

  override def applyLong(value: Long): JValue =
    JLong(value)

  override def unapplyLong(json: JValue): Option[Long] =
    json match {
      case JLong(value) => Some(value)
      case _            => None
    }

  override def applyNull(): JValue =
    JNull

  override def unapplyNull(json: JValue): Boolean =
    json match {
      case JNull => true
      case _     => false
    }

  override def applyObject(appendAllFields: ((String, JValue) => Unit) => Unit): JValue = {
    val fieldsBuilder = List.newBuilder[(String, JValue)]

    appendAllFields { (fieldName, fieldValue) =>
      fieldsBuilder += fieldName -> fieldValue
    }

    JObject(fieldsBuilder.result())
  }

  override def unapplyObject(json: JValue): Option[JObject] =
    json match {
      case fields: JObject => Some(fields)
      case _               => None
    }

  override def applyString(string: String): JValue =
    JString(string)

  override def unapplyString(json: JValue): Option[String] =
    json match {
      case JString(value) => Some(value)
      case _              => None
    }

  override def isObject(json: JValue): Boolean = json.isInstanceOf[JObject]
  override def isArray(json: JValue): Boolean = json.isInstanceOf[JArray]
}

object Json4sOps extends AstOps[JValue, String] with Json4sOpsBase with JsonParser with JsonRenderer
