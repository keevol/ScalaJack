package co.blocke.scalajack

import scala.collection.immutable

//------------------------------------------- Error

sealed trait WriteError

object WriteError {
  case object Nothing extends WriteError
  case class Unsupported(msg: String) extends WriteError
  case class ExceptionThrown(exception: Throwable) extends WriteError
}

//------------------------------------------- Exception

object WriteException {

  private def format(writeFailure: WriteFailure): String = {
    val stringBuilder = new StringBuilder
    val errors = writeFailure.errors

    stringBuilder.append("WriteException")

    errors.size match {
      case 0 => stringBuilder.append("(no errors)")
      case 1 => stringBuilder.append("(1 error):")
      case n => stringBuilder.append(s"($n errors):")
    }

    errors.foreach(e => stringBuilder.append(s"\n  $e"))

    stringBuilder.result()
  }

}

class WriteException(val writeFailure: WriteFailure) extends RuntimeException(WriteException.format(writeFailure))

//------------------------------------------- Result

sealed trait WriteResult[+J] {
  def get: J
  def map[JJ](f: J => JJ): WriteResult[JJ]
  def errors: immutable.Seq[WriteError]
}

case class WriteSuccess[+J](get: J) extends WriteResult[J] {
  override def map[JJ](f: J => JJ): WriteResult[JJ] = WriteSuccess(f(get))
  override def errors: immutable.Seq[WriteError] = immutable.Seq.empty
}

object WriteFailure {
  def apply[J](errors: WriteError*): WriteFailure = WriteFailure(errors.to[immutable.Seq])
}

case class WriteFailure(errors: immutable.Seq[WriteError]) extends WriteResult[Nothing] {
  override def get: Nothing = throw new UnsupportedOperationException("WriteFailure.get not supported")
  override def map[JJ](f: Nothing => JJ): WriteResult[JJ] = this.asInstanceOf[WriteResult[JJ]]
  def isNothing: Boolean = errors.contains(WriteError.Nothing)
  override def toString: String = productPrefix + errors.mkString("(", ", ", ")")
}
