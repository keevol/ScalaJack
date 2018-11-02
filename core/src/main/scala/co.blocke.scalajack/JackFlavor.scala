package co.blocke.scalajack

trait JackFlavor[AST, S] {
  implicit val guidance: SerializationGuidance
  implicit val ops: AstOps[AST, S]

  def render[T](instance: T)(implicit tt: TypeTag[T]): S

  // No exceptions on failure -- Left return on Either for failures
  def readSafely[T](src: S)(implicit tt: TypeTag[T]): Either[DeserializationFailure, T]

  def read[T](src: S)(implicit tt: TypeTag[T]): T =
    readSafely[T](src) match {
      case Right(x) => x
      case Left(x)  => throw new DeserializationException(x)
    }

  def parseToAST(src: S): AST
  def emitFromAST(ast: AST): S

  def materialize[T](ast: AST)(implicit tt: TypeTag[T]): T
  def dematerialize[T](t: T)(implicit tt: TypeTag[T]): AST

  // TODO
  //  def become[N](ast: AST)(implicit becomeFn: (AST) => N): N = becomeFn(ast)
}
