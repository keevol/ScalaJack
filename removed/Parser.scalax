package co.blocke.scalajack

trait Parser[S] {
  def _parse[AST](source: S)(implicit ops: AstOps[AST, S]): Option[AST]
}

