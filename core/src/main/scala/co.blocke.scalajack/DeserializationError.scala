package co.blocke.scalajack

sealed trait DeserializationError {

  def message: String
  def reportedBy: Deserializer[_]
  override def toString: String = s"$message (reported by: ${reportedBy.toString.split('@')(0)})"

}

object DeserializationError {

  case class Missing(reportedBy: Deserializer[_]) extends DeserializationError {
    override def message: String = s"Required field missing"
  }

  case class ExceptionThrown(exception: Throwable) extends DeserializationError {
    override def message: String = s"Exception was thrown: $exception"
    override def reportedBy: Deserializer[_] = new Deserializer[Any] { // bogus Deserializer...exists to print "unnown" in error message
      override def toString: String = "unknown"
      // $COVERAGE-OFF$Never used
      def deserialize[AST, S](path: Path, ast: AST)(implicit ops: AstOps[AST, S], guidance: SerializationGuidance): DeserializationResult[Any] = ???
      // $COVERAGE-ON$
    }
  }

  case class Unsupported(message: String, reportedBy: Deserializer[_]) extends DeserializationError

  case class Unexpected(message: String, reportedBy: Deserializer[_]) extends DeserializationError

  object Malformed {

    def apply(cause: Throwable, reportedBy: Deserializer[_]): DeserializationError =
      new Malformed(message    = cause.getMessage, reportedBy = reportedBy)

  }

  case class Malformed(message: String, reportedBy: Deserializer[_]) extends DeserializationError

}
