package co.blocke.scalajack

trait Ops[IR, WIRE] extends OpsBase[IR] with WireTransceiver[IR, WIRE]

trait OpsBase[IR] {

  self =>

  implicit val selfOps = self

  type ObjectType
  type ArrayType

  def applyArray(value: Seq[IR]): IR
  def unapplyArray(ir: IR): Option[Seq[IR]]

  def applyBoolean(value: Boolean): IR
  def unapplyBoolean(ir: IR): Option[Boolean]

  def applyDecimal(value: BigDecimal): IR
  def unapplyDecimal(ir: IR): Option[BigDecimal]

  def applyDouble(value: Double): IR
  def unapplyDouble(ir: IR): Option[Double]

  def applyInt(value: BigInt): IR
  def unapplyInt(ir: IR): Option[BigInt]

  def applyLong(value: Long): IR
  def unapplyLong(ir: IR): Option[Long]

  def applyNull(): IR
  def unapplyNull(ir: IR): Boolean

  def applyObject(elements: Seq[(String, IR)]): IR
  def unapplyObject(ir: IR): Option[Seq[(String, IR)]]

  def applyString(value: String): IR
  def unapplyString(ir: IR): Option[String]

  def getObjectField(obj: ObjectType, name: String): Option[IR]
  def getArrayElement(arr: ArrayType, index: Int): Option[IR]

  def isObject(ir: IR): Boolean
  def isArray(ir: IR): Boolean

  //------- Operations you get "for free", i.e. no need to implement these for a new AST

  def mapArray[A](arr: IR, fn: (IR, Int) => A): Seq[A] = arr match {
    case IRArray(elements) => elements.zipWithIndex.map { case (ir, index) => fn(ir, index) }
    case _                 => throw new IllegalArgumentException("mapArray() requires IRArray as input")
  }

  def mapObject[A](obj: IR, fn: (String, IR) => A): List[A] = obj match {
    case IRObject(elements) => elements.map { case (name, value) => fn(name, value) }.toList
    case _                  => throw new IllegalArgumentException("mapObject() requires IRObject as input")
  }

  def mutateObject(obj: IR, fn: (String, IR) => (String, IR)): ObjectType = obj match {
    case IRObject(elements) => IRObject(elements.map { case (name, value) => fn(name, value) }).asInstanceOf[ObjectType]
    case _                  => throw new IllegalArgumentException("mutateObject() requires IRObject as input")
  }

  def partitionObject(obj: IR, fn: (String, IR) => Boolean): (ObjectType, ObjectType) = obj match {
    case IRObject(elements) =>
      val m1 = scala.collection.mutable.ListBuffer.empty[(String, IR)]
      val m2 = scala.collection.mutable.ListBuffer.empty[(String, IR)]
      elements.map(e => if (fn(e._1, e._2)) m1 += e else m2 += e)
      (IRObject(m1).asInstanceOf[ObjectType], IRObject(m2).asInstanceOf[ObjectType])
    case _ => throw new IllegalArgumentException("partitionObject() requires IRObject as input")
  }

  def become[IRB](source: IR)(implicit targetOps: OpsBase[IRB]): IRB =
    if (this == targetOps) {
      source.asInstanceOf[IRB]
    } else {
      source match {
        case IRArray(elements)       => IRArray[IRB](elements.map(e => become(e)(targetOps)))
        case IRBoolean(booleanValue) => IRBoolean[IRB](booleanValue)
        case IRDecimal(bigDecimal)   => IRDecimal[IRB](bigDecimal)
        case IRDouble(doubleValue)   => IRDouble[IRB](doubleValue)
        case IRInt(bigInt)           => IRInt[IRB](bigInt)
        case IRLong(longValue)       => IRLong[IRB](longValue)
        case IRNull()                => IRNull[IRB]()
        case IRObject(elements)      => IRObject[IRB](elements.map { case (k, v) => (k, become(v)(targetOps)) })
        case IRString(string)        => IRString[IRB](string)
      }
    }
}

object Ops {
  type Aux[IR, WIRE, OBJ] = Ops[IR, WIRE] { type ObjectType = OBJ }
}

// The class, objects, and traits below are used exclusively to support SJCapture capability
//-----------------------------------
trait IRAndOps {
  type ObjectType
  type IRType
  type WireType
  val capturedFields: ObjectType
  implicit val ops: Ops[IRType, WireType]
}

object IRAndOps {
  def apply[IR, WIRE, OBJ](captured: OBJ)(implicit ops: Ops.Aux[IR, WIRE, OBJ]): IRAndOps =
    new IRAndOps {
      override type ObjectType = OBJ
      override type IRType = IR
      override type WireType = WIRE
      override val capturedFields: ObjectType = captured
      override implicit val ops: Ops[IRType, WireType] = ops
    }
}
