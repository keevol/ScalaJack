package co.blocke.scalajack

trait JackFlavor[IR, WIRE] {

  this: ScalaJackLike[IR, WIRE] =>

  implicit val guidance: SerializationGuidance
  implicit val ops: Ops[IR, WIRE]

  def render[T](instance: T)(implicit tt: TypeTag[T]): WIRE

  // No exceptions on failure -- Left return on Either for failures
  def readSafely[T](wire: WIRE)(implicit tt: TypeTag[T]): Either[ReadFailure, T] = {
    val irTransceiver = context.typeAdapterOf[T].irTransceiver
    ops.deserialize(Path.Root, wire).mapToReadResult(Path.Root, (dsIR: IR) => irTransceiver.read(Path.Root, dsIR)) match {
      case rs: ReadSuccess[T] => Right(rs.get)
      case rf: ReadFailure    => Left(rf)
    }
  }

  def read[T](wire: WIRE)(implicit tt: TypeTag[T]): T =
    readSafely[T](wire) match {
      case Right(x) => x
      case Left(x)  => throw new ReadException(x)
    }

  def parse(src: WIRE): DeserializationResult[IR] = ops.deserialize(Path.Root, src)
  def emit(ir: IR): WIRE = ops.serialize(ir, this)

  def materialize[T](ir: IR)(implicit tt: TypeTag[T]): ReadResult[T] =
    context.typeAdapterOf[T].irTransceiver.read(Path.Root, ir)

  def dematerialize[T](t: T)(implicit tt: TypeTag[T]): WriteResult[IR] = {
    context.typeAdapterOf[T].irTransceiver.write(TypeTagged(t, typeOf[T]))(ops, guidance) match {
      case res: WriteSuccess[_] => res
      // $COVERAGE-OFF$Don't know how to trigger this
      case fail: WriteFailure   => fail
      // $COVERAGE-ON$
    }
  }
}
