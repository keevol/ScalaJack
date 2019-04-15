package co.blocke.scalajack

trait _MultiKind // marker trait

case class MultiKind2[A, B](a: Option[A], b: Option[B]) extends _MultiKind {
  def unpack: Any = a.getOrElse(b.get)
  def _unpack: (Any, Int) = a.map(v => (v, 0)).getOrElse((b.get, 1))
}

case class MultiKind3[A, B, C](a: Option[A], b: Option[B], c: Option[C]) extends _MultiKind {
  def unpack: Any = a.getOrElse(b.getOrElse(c.get))
  def _unpack: (Any, Int) = a.map((_, 0)).getOrElse(b.map((_, 1)).getOrElse((c.get, 2)))
}

case class MultiKind4[A, B, C, D](a: Option[A], b: Option[B], c: Option[C], d: Option[D]) extends _MultiKind {
  def unpack: Any = a.getOrElse(b.getOrElse(c.getOrElse(d.get)))
  def _unpack: (Any, Int) = a.map((_, 0)).getOrElse(b.map((_, 1)).getOrElse(c.map((_, 2)).getOrElse((d.get, 3))))
}
