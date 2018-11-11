package co.blocke.scalajack
package ast

trait AstOps[AST] {

  self =>

  implicit val selfOps = self

  def applyArray(value: List[AST]): AST
  def unapplyArray(ast: AST): Option[List[AST]]

  def applyBoolean(value: Boolean): AST
  def unapplyBoolean(ast: AST): Option[Boolean]

  def applyDecimal(value: BigDecimal): AST
  def unapplyDecimal(ast: AST): Option[BigDecimal]

  def applyDouble(value: Double): AST
  def unapplyDouble(ast: AST): Option[Double]

  def applyInt(value: BigInt): AST
  def unapplyInt(ast: AST): Option[BigInt]

  def applyLong(value: Long): AST
  def unapplyLong(ast: AST): Option[Long]

  def applyNull(): AST
  // There is no unapply for Null

  def applyObject(elements: Map[String, AST]): AST
  def unapplyObject(ast: AST): Option[Map[String, AST]]

  def applyString(value: String): AST
  def unapplyString(ast: AST): Option[String]

  def getObjectField(obj: AST, name: String): Option[AST] = obj match {
    case AstObject(a) => a.get(name)
    case _            => None
  }
  def getArrayElement(arr: AST, index: Int): Option[AST] = arr match {
    case AstArray(a) if index < a.size => Some(a(index))
    case _                             => None
  }

  def partitionObject(obj: AST, fn: (String, AST) => Boolean): (AST, AST) = obj match {
    case AstObject(elements) =>
      val m1 = scala.collection.mutable.Map.empty[String, AST]
      val m2 = scala.collection.mutable.Map.empty[String, AST]
      elements.map(e => if (fn(e._1, e._2)) m1 += e else m2 += e)
      (AstObject(m1.toMap), AstObject(m2.toMap))
    case _ => throw new IllegalArgumentException("partitionObject() requires AstObject as input")
  }
}

object AstValue {

  def transform[ASTA, ASTB](source: ASTA)(implicit sourceOps: AstOps[ASTA], targetOps: AstOps[ASTB]): ASTB =
    if (sourceOps == targetOps) {
      source.asInstanceOf[ASTB]
    } else {
      source match {
        case AstArray(elements)       => AstArray[ASTB](elements.map(e => transform(e)(sourceOps, targetOps)))
        case AstBoolean(booleanValue) => AstBoolean[ASTB](booleanValue)
        case AstDecimal(bigDecimal)   => AstDecimal[ASTB](bigDecimal)
        case AstDouble(doubleValue)   => AstDouble[ASTB](doubleValue)
        case AstInt(bigInt)           => AstInt[ASTB](bigInt)
        case AstLong(longValue)       => AstLong[ASTB](longValue)
        case AstNull                  => AstNull[ASTB]()
        case AstObject(elements)      => AstObject[ASTB](elements.map { case (k, v) => (k, transform(v)(sourceOps, targetOps)) })
        case AstString(string)        => AstString[ASTB](string)
      }
    }

}

object AstArray {
  @inline final def apply[AST](elements: List[AST])(implicit ops: AstOps[AST]): AST = ops.applyArray(elements)
  @inline final def unapply[AST](ast: AST)(implicit ops: AstOps[AST]): Option[List[AST]] = ops.unapplyArray(ast)
}

object AstBoolean {
  @inline final def apply[AST](value: Boolean)(implicit ops: AstOps[AST]): AST = ops.applyBoolean(value)
  @inline final def unapply[AST](ast: AST)(implicit ops: AstOps[AST]): Option[Boolean] = ops.unapplyBoolean(ast)
}

// Wrapper for custom types utilizing an AstArray tripple:
//  ("AstCustom", typeName, AST)
// The kind of AST in the 3rd element depends on the typeName and is managed with the TypeAdapter.
object AstCustom {
  @inline final def apply[AST](typeName: String, value:AST)(implicit ops: AstOps[AST]): AST =
    ops.applyArray(List(AstString("AstCustom"), AstString(typeName), value))
  @inline final def unapply[AST](ast: AST)(implicit ops: AstOps[AST]): Option[(String,AST)] = ast match {
    case AstArray(a) if(a.size == 3 && a(0) == AstString("AstCustom")) => Some( (ops.unapplyString(a(1)), a(2)) )
    case _ => None
  }
}

object AstDecimal {
  @inline final def apply[AST](value: BigDecimal)(implicit ops: AstOps[AST]): AST = ops.applyDecimal(value)
  @inline final def unapply[AST](ast: AST)(implicit ops: AstOps[AST]): Option[BigDecimal] = ops.unapplyDecimal(ast)
}

object AstDouble {
  @inline final def apply[AST](value: Double)(implicit ops: AstOps[AST]): AST = ops.applyDouble(value)
  @inline final def unapply[AST](ast: AST)(implicit ops: AstOps[AST]): Option[Double] = ops.unapplyDouble(ast)
}

object AstInt {
  @inline final def apply[AST](value: BigInt)(implicit ops: AstOps[AST]): AST = ops.applyInt(value)
  @inline final def unapply[AST](ast: AST)(implicit ops: AstOps[AST]): Option[BigInt] = ops.unapplyInt(ast)
}

object AstLong {
  @inline final def apply[AST](value: Long)(implicit ops: AstOps[AST]): AST = ops.applyLong(value)
  @inline final def unapply[AST](ast: AST)(implicit ops: AstOps[AST]): Option[Long] = ops.unapplyLong(ast)
}

object AstNull {
  @inline final def apply[AST]()(implicit ops: AstOps[AST]): AST = ops.applyNull()
}

object AstObject {
  @inline final def apply[AST](elements: Map[String, AST])(implicit ops: AstOps[AST]): AST = ops.applyObject(elements)
  @inline final def unapply[AST](ast: AST)(implicit ops: AstOps[AST]): Option[Map[String, AST]] = ops.unapplyObject(ast)
}

object AstString {
  @inline final def apply[AST](value: String)(implicit ops: AstOps[AST]): AST = ops.applyString(value)
  @inline final def unapply[AST](ast: AST)(implicit ops: AstOps[AST]): Option[String] = ops.unapplyString(ast)
}
