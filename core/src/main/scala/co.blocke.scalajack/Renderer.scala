package co.blocke.scalajack

trait Renderer[S] {
  def _renderCompact[AST](ast: AST, sj: ScalaJackLike[_, _])(implicit ops: AstOps[AST, S]): S
}
