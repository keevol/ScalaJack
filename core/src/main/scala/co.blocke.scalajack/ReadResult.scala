package co.blocke.scalajack

import scala.collection.immutable
import scala.util.control.NonFatal

//------------------------------------------- Error

object ReadError {

  case class Missing(absentFieldName: String, reportedBy: IRTransceiver[_]) extends ReadError {
    override def message: String = s"Required field $absentFieldName missing"
  }

  case class ExceptionThrown(exception: Throwable) extends ReadError {
    override def message: String = s"Exception was thrown: $exception"
    override def reportedBy: IRTransceiver[_] = new IRTransceiver[Any] { // bogus Deserializer...exists to print "unnown" in error message
      override def toString: String = "unknown"
    }
  }

  case class Unsupported(message: String, reportedBy: IRTransceiver[_]) extends ReadError
  case class Unexpected(message: String, reportedBy: IRTransceiver[_]) extends ReadError

  object Malformed {
    def apply(cause: Throwable, reportedBy: IRTransceiver[_]): ReadError =
      new Malformed(message    = cause.getMessage, reportedBy = reportedBy)
  }
  case class Malformed(message: String, reportedBy: IRTransceiver[_]) extends ReadError

}

sealed trait ReadError {
  def message: String
  def reportedBy: IRTransceiver[_]
  override def toString: String = s"$message (reported by: ${reportedBy.toString.split('@')(0)})"
}

//------------------------------------------- Exception

object ReadException {

  private def format(readFailure: ReadFailure): String = {
    val stringBuilder = new StringBuilder
    val errors = readFailure.errors

    stringBuilder.append("ReadException")

    errors.size match {
      case 0 => stringBuilder.append("(no errors)")
      case 1 => stringBuilder.append("(1 error):")
      case n => stringBuilder.append(s"($n errors):")
    }

    for ((path, error) <- errors)
      stringBuilder.append(s"\n  [$path] $error")

    stringBuilder.result()
  }

}

class ReadException(val readFailure: ReadFailure) extends RuntimeException(ReadException.format(readFailure))

//------------------------------------------- Result

object ReadResult {

  def apply[T](path: Path)(body: => TypeTagged[T], readError: PartialFunction[Throwable, ReadError] = PartialFunction.empty): ReadResult[T] =
    try ReadSuccess(body) catch {
      case e: ReadException => e.readFailure
      case NonFatal(e)      => ReadFailure(path, readError.applyOrElse(e, ReadError.ExceptionThrown))
    }
}

sealed trait ReadResult[+T] {
  def get: TypeTagged[T]
  def errors: immutable.Seq[(Path, ReadError)]
  def map[U](f: TypeTagged[T] => TypeTagged[U]): ReadResult[U]
  def flatMap[U](f: TypeTagged[T] => ReadResult[U]): ReadResult[U]
  def isSuccess: Boolean
  def isFailure: Boolean
}

case class ReadSuccess[+T](get: TypeTagged[T]) extends ReadResult[T] {
  override def errors: immutable.Seq[(Path, ReadError)] = immutable.Seq.empty
  override def map[U](f: TypeTagged[T] => TypeTagged[U]): ReadResult[U] = ReadSuccess(f(get))
  override def flatMap[U](f: TypeTagged[T] => ReadResult[U]): ReadResult[U] = f(get)
  override def isSuccess: Boolean = true
  override def isFailure: Boolean = false
}

object ReadFailure {
  def apply[T](path: Path, errors: ReadError*): ReadFailure = new ReadFailure(errors.map(error => (path, error)).to[immutable.Seq])
}

case class ReadFailure(errors: immutable.Seq[(Path, ReadError)]) extends ReadResult[Nothing] {

  override def get: TypeTagged[Nothing] = throw new ReadException(this)
  override def map[U](f: TypeTagged[Nothing] => TypeTagged[U]): ReadResult[U] = this.asInstanceOf[ReadResult[U]]
  override def flatMap[U](f: TypeTagged[Nothing] => ReadResult[U]): ReadResult[U] = this.asInstanceOf[ReadResult[U]]

  def errors(path: Path): immutable.Seq[(Path, ReadError)] = errors.filter(_._1 == path)

  def isUnsupported(path: Path): Boolean = errors(path).exists(_._2.isInstanceOf[ReadError.Unsupported])
  def isUnexpected(path: Path): Boolean = errors(path).exists(_._2.isInstanceOf[ReadError.Unexpected])
  override def isSuccess: Boolean = false
  override def isFailure: Boolean = true
}

