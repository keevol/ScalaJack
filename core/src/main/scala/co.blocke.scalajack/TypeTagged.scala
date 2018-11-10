package co.blocke.scalajack

object TypeTagged {

  val BooleanType: Type = typeOf[Boolean]
  val ByteType: Type = typeOf[Byte]
  val CharType: Type = typeOf[Char]
  val DoubleType: Type = typeOf[Double]
  val FloatType: Type = typeOf[Float]
  val IntType: Type = typeOf[Int]
  val LongType: Type = typeOf[Long]
  val ShortType: Type = typeOf[Short]

  /**
   * Enables pattern matching.
   */
  @inline final def unapply[T](tagged: TypeTagged[T]): tagged.type = tagged

  def inferFromRuntimeClass[T](value: T): TypeTagged[T] =
    new TypeTagged[T] {

      override def get: T = value

      override lazy val tpe: Type = classSymbol(value.getClass).asType.toType

    }

  def apply[T](value: T, valueType: Type): TypeTagged[T] = Fixed(value, valueType)

  def apply(booleanValue: Boolean): TypeTagged[Boolean] = TypeTagged(booleanValue, typeOf[Boolean])
  def apply(byteValue: Byte): TypeTagged[Byte] = TypeTagged[Byte](byteValue, typeOf[Byte])
  def apply(charValue: Char): TypeTagged[Char] = TypeTagged[Char](charValue, typeOf[Char])
  def apply(doubleValue: Double): TypeTagged[Double] = TypeTagged[Double](doubleValue, typeOf[Double])
  def apply(floatValue: Float): TypeTagged[Float] = TypeTagged[Float](floatValue, typeOf[Float])
  def apply(intValue: Int): TypeTagged[Int] = TypeTagged[Int](intValue, typeOf[Int])
  def apply(longValue: Long): TypeTagged[Long] = TypeTagged[Long](longValue, typeOf[Long])
  def apply(shortValue: Short): TypeTagged[Short] = TypeTagged[Short](shortValue, typeOf[Short])

  private case class Fixed[+T](get: T, tpe: Type) extends TypeTagged[T]

}

/**
 * A combination of a value and its known [[Type]].
 *
 * @tparam T
 */
trait TypeTagged[+T] {

  def get: T

  def tpe: Type

  override def toString: String = s"$get as $tpe"

  /**
   * Used to fulfill the requirements of [[TypeTagged#unapply]].
   */
  @inline final def isEmpty: Boolean = false

}
