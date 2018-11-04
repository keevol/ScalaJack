package co.blocke.scalajack
package dynamodb

import co.blocke.scalajack.json.Json4sOps
import com.amazonaws.services.dynamodbv2.document.Item
import org.json4s.JsonAST.{ JArray, JBool, JDecimal, JDouble, JInt, JLong, JNull, JObject, JString, JValue }

object DynamoOps extends AstOps[JValue, Item] {

  val jsonOps: AstOps[JValue, String] = Json4sOps.asInstanceOf[AstOps[JValue, String]]

  override type ArrayElements = jsonOps.ArrayElements
  override type ObjectFields = jsonOps.ObjectFields

  def foreachArrayElement(elements: ArrayElements, f: (Int, JValue) => Unit): Unit =
    jsonOps.foreachArrayElement(elements.asInstanceOf[jsonOps.ArrayElements], f)

  def foreachObjectField(fields: ObjectFields, f: (String, JValue) => Unit): Unit =
    jsonOps.foreachObjectField(fields.asInstanceOf[jsonOps.ObjectFields], f)

  def getObjectField(fields: ObjectFields, name: String): Option[JValue] =
    jsonOps.getObjectField(fields.asInstanceOf[jsonOps.ObjectFields], name: String)

  def partitionObjectFields(fields: ObjectFields, fieldNames: List[String]): (ObjectFields, ObjectFields) =
    jsonOps.partitionObjectFields(fields.asInstanceOf[jsonOps.ObjectFields], fieldNames: List[String])

  def applyArray(appendAllElements: (JValue => Unit) => Unit): JValue =
    jsonOps.applyArray(appendAllElements)

  def unapplyArray(json: JValue): Option[ArrayElements] =
    jsonOps.unapplyArray(json).asInstanceOf[Option[ArrayElements]]

  def applyBoolean(value: Boolean): JValue = JBool(value)
  def unapplyBoolean(json: JValue): Option[Boolean] = jsonOps.unapplyBoolean(json)

  def applyDecimal(value: BigDecimal): JValue = JDecimal(value)
  def unapplyDecimal(json: JValue): Option[BigDecimal] = jsonOps.unapplyDecimal(json)

  def applyDouble(value: Double): JValue = JDouble(value)
  def unapplyDouble(json: JValue): Option[Double] = jsonOps.unapplyDouble(json)

  def applyInt(value: BigInt): JValue = JInt(value)
  def unapplyInt(json: JValue): Option[BigInt] = jsonOps.unapplyInt(json)

  def applyLong(value: Long): JValue = JLong(value)
  def unapplyLong(json: JValue): Option[Long] = jsonOps.unapplyLong(json)

  def applyNull(): JValue = JNull
  def unapplyNull(json: JValue): Boolean = jsonOps.unapplyNull(json)

  def applyObject(appendAllFields: ((String, JValue) => Unit) => Unit): JValue =
    jsonOps.applyObject(appendAllFields)

  def unapplyObject(json: JValue): Option[ObjectFields] =
    jsonOps.unapplyObject(json).asInstanceOf[Option[ObjectFields]]

  def applyString(string: String): JValue = JString(string)
  def unapplyString(json: JValue): Option[String] = jsonOps.unapplyString(json)

  def isObject(json: JValue): Boolean = json.isInstanceOf[JObject]
  def isArray(json: JValue): Boolean = json.isInstanceOf[JArray]

  val parser: Parser[Item] = new Parser[Item] {
    def parse[AST](source: Item)(implicit ops: AstOps[AST, Item]): Option[AST] =
      jsonOps.parser.parse(source.toJSON())(Json4sOps).asInstanceOf[Option[AST]]
  }

  val renderer: Renderer[Item] = new Renderer[Item] {
    def renderCompact[AST](ast: AST, sj: ScalaJackLike[_, _])(implicit ops: AstOps[AST, Item]): Item =
      Item.fromJSON(jsonOps.renderer.renderCompact(ast, sj)(Json4sOps.asInstanceOf[AstOps[AST, String]]))
  }

}
