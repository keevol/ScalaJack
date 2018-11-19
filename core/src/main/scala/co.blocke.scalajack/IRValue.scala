package co.blocke.scalajack

object IRArray {
  @inline final def apply[IR](elements: Seq[IR])(implicit ops: OpsBase[IR]): IR = ops.applyArray(elements)
  @inline final def unapply[IR](ir: IR)(implicit ops: OpsBase[IR]): Option[Seq[IR]] = ops.unapplyArray(ir)
}

object IRBoolean {
  @inline final def apply[IR](value: Boolean)(implicit ops: OpsBase[IR]): IR = ops.applyBoolean(value)
  @inline final def unapply[IR](ir: IR)(implicit ops: OpsBase[IR]): Option[Boolean] = ops.unapplyBoolean(ir)
}

// Pseudo-IR wrapper for custom types utilizing an IRArray tripple: ("IRCustom", typeName, IR)
// The kind of IR in the 3rd element depends on the typeName and is managed with the TypeAdapter.
object IRCustom {
  @inline final def apply[IR](typeName: String, value: IR)(implicit ops: OpsBase[IR]): IR =
    ops.applyArray(List(IRString("IRCustom"), IRString(typeName), value))
  @inline final def unapply[IR](ir: IR)(implicit ops: OpsBase[IR]): Option[(String, IR)] = ir match {
    case IRArray(a) if (a.size == 3 && a(0) == IRString("IRCustom")) => Some((ops.unapplyString(a(1)).get, a(2)))
    case _ => None
  }
}

object IRDecimal {
  @inline final def apply[IR](value: BigDecimal)(implicit ops: OpsBase[IR]): IR = ops.applyDecimal(value)
  @inline final def unapply[IR](ir: IR)(implicit ops: OpsBase[IR]): Option[BigDecimal] = ops.unapplyDecimal(ir)
}

object IRDouble {
  @inline final def apply[IR](value: Double)(implicit ops: OpsBase[IR]): IR = ops.applyDouble(value)
  @inline final def unapply[IR](ir: IR)(implicit ops: OpsBase[IR]): Option[Double] = ops.unapplyDouble(ir)
}

object IRInt {
  @inline final def apply[IR](value: BigInt)(implicit ops: OpsBase[IR]): IR = ops.applyInt(value)
  @inline final def unapply[IR](ir: IR)(implicit ops: OpsBase[IR]): Option[BigInt] = ops.unapplyInt(ir)
}

object IRLong {
  @inline final def apply[IR](value: Long)(implicit ops: OpsBase[IR]): IR = ops.applyLong(value)
  @inline final def unapply[IR](ir: IR)(implicit ops: OpsBase[IR]): Option[Long] = ops.unapplyLong(ir)
}

object IRNull {
  @inline final def apply[IR]()(implicit ops: OpsBase[IR]): IR = ops.applyNull()
  @inline final def unapply[IR](ir: IR)(implicit ops: OpsBase[IR]): Boolean = ops.unapplyNull(ir)
}

object IRObject {
  @inline final def apply[IR](elements: Seq[(String, IR)])(implicit ops: OpsBase[IR]): IR = ops.applyObject(elements)
  @inline final def unapply[IR](ir: IR)(implicit ops: OpsBase[IR]): Option[Seq[(String, IR)]] = ops.unapplyObject(ir)
}

// IRMap is a synthetic IR creation--it doesn't exist in the actual underlying IR, e.g. Json4s.  Json4s is a great
// IR, except it presumes JSON's conventions (understandably), notably a JObject's key is always a String.  Scala Maps
// aren't bounded by this convention, so we need a [IR,IR] representation for Map types.  We "fake" it with this
// synthetic construction, implemented by IR primitives.
object IRMap {
  @inline final def apply[IR](elements: Seq[(IR, IR)])(implicit ops: OpsBase[IR]): IR =
    IRCustom("__ScalaMap", IRArray(elements.map { case (k, v) => IRArray(Seq(k, v)) }))
  @inline final def unapply[IR](ir: IR)(implicit ops: OpsBase[IR]): Option[Seq[(IR, IR)]] =
    ir match {
      case IRCustom("__ScalaMap", IRArray(elements)) =>
        Some(elements.map {
          _ match {
            case IRArray(kvArr) => (kvArr(0), kvArr(1))
          }
        })
      case _ => None
    }
}

object IRString {
  @inline final def apply[IR](value: String)(implicit ops: OpsBase[IR]): IR = ops.applyString(value)
  @inline final def unapply[IR](ir: IR)(implicit ops: OpsBase[IR]): Option[String] = ops.unapplyString(ir)
}
