package co.blocke.scalajack

import scala.collection.immutable
import scala.util.control.NonFatal

//------------------------------------------- Result

sealed trait DeserializationResult[IR] {
  def get: IR
  //  def getOrElse(substitute: IR): IR
  def errors: immutable.Seq[(Path, ReadError)]
  def mapToReadResult[T](path: Path, fn: IR => ReadResult[T]) =
    this match {
      case DeserializationSuccess(ir)   => fn(ir)
      case DeserializationFailure(errs) => ReadFailure(errs)
    }
}

object DeserializationResult {
  def apply[IR](path: Path)(body: => IR, deserializationError: PartialFunction[Throwable, ReadError] = PartialFunction.empty): DeserializationResult[IR] =
    try DeserializationSuccess(body) catch {
      //      case e: ReadException => e.readFailure
      case NonFatal(e) => DeserializationFailure(path, deserializationError.applyOrElse(e, ReadError.ExceptionThrown))
    }
}

case class DeserializationSuccess[IR](get: IR) extends DeserializationResult[IR] {
  override def errors: immutable.Seq[(Path, ReadError)] = immutable.Seq.empty
  //  override def getOrElse(substitute: IR): IR = get
}

object DeserializationFailure {
  def apply[IR](path: Path, errors: ReadError*): DeserializationFailure[IR] =
    DeserializationFailure(errors.map(error => (path, error)).to[immutable.Seq])
}

case class DeserializationFailure[IR](errors: immutable.Seq[(Path, ReadError)]) extends DeserializationResult[IR] {
  override def get: IR = throw new UnsupportedOperationException("DeserializationFailure.get not supported")
  //  override def getOrElse(substitute: IR): IR = substitute
  //  override def toString: String = productPrefix + errors.mkString("(", ", ", ")")
}
