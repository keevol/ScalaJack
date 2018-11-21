package co.blocke.scalajack

object NumberConverters {

  implicit class BigDecimalOps(val bigDecimal: BigDecimal) extends AnyVal {

    def toDoubleExact: Double = {
      val bigDecimalAsDouble: Double = bigDecimal.toDouble
      val bigDecimalAsDoubleAsBigDecimal: BigDecimal = BigDecimal.decimal(bigDecimalAsDouble)
      if (bigDecimal == bigDecimalAsDoubleAsBigDecimal) {
        bigDecimalAsDouble
      } else {
        // $COVERAGE-OFF$Not sure how to trigger this
        throw new ArithmeticException(s"$bigDecimal (BigDecimal) cannot be exactly converted to Double ($bigDecimalAsDouble)")
        // $COVERAGE-ON$
      }
    }

    def toFloatExact: Float = {
      val bigDecimalAsFloat: Float = bigDecimal.toFloat
      val bigDecimalAsFloatAsBigDecimal: BigDecimal = BigDecimal.decimal(bigDecimalAsFloat)
      if (bigDecimal == bigDecimalAsFloatAsBigDecimal) {
        bigDecimalAsFloat
      } else {
        throw new ArithmeticException(s"$bigDecimal (BigDecimal) cannot be exactly converted to Float ($bigDecimalAsFloat)")
      }
    }

    // $COVERAGE-OFF$Can't for the life of me convince coveralls this is tested.  It is!  So just block it out so it doesn't break ny stats
    def toShortExact: Short = {
      val bigDecimalAsShort: Short = bigDecimal.toShort
      val bigDecimalAsShortAsBigDecimal: BigDecimal = BigDecimal(bigDecimalAsShort)
      if (bigDecimal == bigDecimalAsShortAsBigDecimal) {
        bigDecimalAsShort
      } else {
        throw new ArithmeticException(s"$bigDecimal (BigDecimal) cannot be exactly converted to Short ($bigDecimalAsShort)")
      }
    }
    // $COVERAGE-ON$

  }

  implicit class BigIntOps(val bigInt: BigInt) extends AnyVal {
    def toShortExact: Short = {
      val bigIntAsShort: Short = bigInt.toShort
      val bigIntAsShortAsBigInt: BigInt = BigInt(bigIntAsShort)
      if (bigInt == bigIntAsShortAsBigInt) {
        bigIntAsShort
      } else {
        // $COVERAGE-OFF$Not sure how to trigger this
        throw new ArithmeticException(s"$bigInt (BigInt) cannot be exactly converted to Short ($bigIntAsShort)")
        // $COVERAGE-ON$
      }
    }
  }

  implicit class DoubleOps(val double: Double) extends AnyVal {

    def toFloatExact: Float = {
      val doubleAsFloat: Float = double.floatValue
      val doubleAsFloatAsDouble: Double = doubleAsFloat.doubleValue
      if (double == doubleAsFloatAsDouble) {
        doubleAsFloat
      } else {
        throw new ArithmeticException(s"$double (Double) cannot be exactly converted to $doubleAsFloat (Float)")
      }
    }

  }

  implicit class LongOps(val long: Long) extends AnyVal {

    def toFloatExact: Float = {
      val longAsFloat: Float = long.floatValue
      val longAsFloatAsLong: Long = longAsFloat.longValue
      if (long == longAsFloatAsLong) {
        longAsFloat
      } else {
        // $COVERAGE-OFF$Not sure how to trigger this
        throw new ArithmeticException(s"$long (Long) cannot be exactly converted to Float ($longAsFloat)")
        // $COVERAGE-ON$
      }
    }

    def toIntExact: Int = {
      val longAsInt: Int = long.toInt
      val longAsIntAsLong: Long = longAsInt.toLong
      if (long == longAsIntAsLong) {
        longAsInt
      } else {
        throw new ArithmeticException(s"$long (Long) cannot be exactly converted to Int ($longAsInt)")
      }
    }

    def toShortExact: Short = {
      val longAsShort: Short = long.toShort
      val longAsShortAsLong: Long = longAsShort.toLong
      if (long == longAsShortAsLong) {
        longAsShort
      } else {
        throw new ArithmeticException(s"$long (Long) cannot be exactly converted to Short ($longAsShort)")
      }
    }

  }

}
