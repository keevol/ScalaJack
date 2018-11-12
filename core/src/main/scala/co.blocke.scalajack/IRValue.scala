package co.blocke.scalajack

object IRArray {
  @inline final def apply[IR](elements: Seq[IR])(implicit ops: OpsBase[IR]): IR = ops.applyArray(elements)
  @inline final def unapply[IR](ast: IR)(implicit ops: OpsBase[IR]): Option[Seq[IR]] = ops.unapplyArray(ast)
}

object IRBoolean {
  @inline final def apply[IR](value: Boolean)(implicit ops: OpsBase[IR]): IR = ops.applyBoolean(value)
  @inline final def unapply[IR](ast: IR)(implicit ops: OpsBase[IR]): Option[Boolean] = ops.unapplyBoolean(ast)
}

// Pseudo-IR wrapper for custom types utilizing an IRArray tripple: ("IRCustom", typeName, IR)
// The kind of IR in the 3rd element depends on the typeName and is managed with the TypeAdapter.
object IRCustom {
  @inline final def apply[IR](typeName: String, value: IR)(implicit ops: OpsBase[IR]): IR =
    ops.applyArray(List(IRString("IRCustom"), IRString(typeName), value))
  @inline final def unapply[IR](ast: IR)(implicit ops: OpsBase[IR]): Option[(String, IR)] = ast match {
    case IRArray(a) if (a.size == 3 && a(0) == IRString("IRCustom")) => Some((ops.unapplyString(a(1)).get, a(2)))
    case _ => None
  }
}

object IRDecimal {
  @inline final def apply[IR](value: BigDecimal)(implicit ops: OpsBase[IR]): IR = ops.applyDecimal(value)
  @inline final def unapply[IR](ast: IR)(implicit ops: OpsBase[IR]): Option[BigDecimal] = ops.unapplyDecimal(ast)
}

object IRDouble {
  @inline final def apply[IR](value: Double)(implicit ops: OpsBase[IR]): IR = ops.applyDouble(value)
  @inline final def unapply[IR](ast: IR)(implicit ops: OpsBase[IR]): Option[Double] = ops.unapplyDouble(ast)
}

object IRInt {
  @inline final def apply[IR](value: BigInt)(implicit ops: OpsBase[IR]): IR = ops.applyInt(value)
  @inline final def unapply[IR](ast: IR)(implicit ops: OpsBase[IR]): Option[BigInt] = ops.unapplyInt(ast)
}

object IRLong {
  @inline final def apply[IR](value: Long)(implicit ops: OpsBase[IR]): IR = ops.applyLong(value)
  @inline final def unapply[IR](ast: IR)(implicit ops: OpsBase[IR]): Option[Long] = ops.unapplyLong(ast)
}

object IRNull {
  @inline final def apply[IR]()(implicit ops: OpsBase[IR]): IR = ops.applyNull()
  @inline final def unapply[IR](ast: IR)(implicit ops: OpsBase[IR]): Boolean = ops.unapplyNull(ast)
}

object IRObject {
  @inline final def apply[IR](elements: Seq[(String, IR)])(implicit ops: OpsBase[IR]): IR = ops.applyObject(elements)
  @inline final def unapply[IR](ast: IR)(implicit ops: OpsBase[IR]): Option[Seq[(String, IR)]] = ops.unapplyObject(ast)
}

object IRString {
  @inline final def apply[IR](value: String)(implicit ops: OpsBase[IR]): IR = ops.applyString(value)
  @inline final def unapply[IR](ast: IR)(implicit ops: OpsBase[IR]): Option[String] = ops.unapplyString(ast)
}
