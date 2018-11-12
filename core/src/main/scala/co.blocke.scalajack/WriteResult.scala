package co.blocke.scalajack

import scala.collection.immutable

//------------------------------------------- Error

sealed trait WriteError

object WriteError {
  case object Nothing extends WriteError
  case class Unsupported(msg: String) extends WriteError
  case class ExceptionThrown(exception: Throwable) extends WriteError
}

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
  def apply[J](errors: WriteError*): WriteFailure[J] = WriteFailure[J](errors.to[immutable.Seq])
}

case class WriteFailure[+J](errors: immutable.Seq[WriteError]) extends WriteResult[J] {
  override def get: J = throw new UnsupportedOperationException("WriteFailure.get not supported")
  override def map[JJ](f: J => JJ): WriteResult[JJ] = this.asInstanceOf[WriteResult[JJ]]
  def isNothing: Boolean = errors.contains(WriteError.Nothing)
  override def toString: String = productPrefix + errors.mkString("(", ", ", ")")
}
