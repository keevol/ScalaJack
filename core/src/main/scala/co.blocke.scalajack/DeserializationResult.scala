package co.blocke.scalajack

import scala.collection.immutable
import scala.util.control.NonFatal

//------------------------------------------- Error

//sealed trait DeserializationError
//
//object DeserializationError {
//  case object Nothing extends DeserializationError
////  case class Unsupported(msg: String) extends DeserializationError
//  case class ExceptionThrown(exception: Throwable) extends DeserializationError
//}

//------------------------------------------- Result

sealed trait DeserializationResult[IR] {
  def get: IR
  def errors: immutable.Seq[ReadError]
}

object DeserializationResult {
  def apply[IR](body: => IR, deserializationError: PartialFunction[Throwable, ReadError] = PartialFunction.empty): DeserializationResult[IR] =
    try DeserializationSuccess(body) catch {
      //      case e: ReadException => e.readFailure
      case NonFatal(e) => DeserializationFailure(deserializationError.applyOrElse(e, ReadError.ExceptionThrown))
    }
}

case class DeserializationSuccess[IR](get: IR) extends DeserializationResult[IR] {
  override def errors: immutable.Seq[ReadError] = immutable.Seq.empty
}

object DeserializationFailure {
  def apply[IR](errors: ReadError*): DeserializationFailure[IR] = DeserializationFailure(errors.to[immutable.Seq])
}

case class DeserializationFailure[IR](errors: immutable.Seq[ReadError]) extends DeserializationResult[IR] {
  override def get: IR = throw new UnsupportedOperationException("DeserializationFailure.get not supported")
  override def toString: String = productPrefix + errors.mkString("(", ", ", ")")
}

