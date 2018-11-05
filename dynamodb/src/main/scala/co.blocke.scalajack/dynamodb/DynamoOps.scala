package co.blocke.scalajack
package dynamodb

import co.blocke.scalajack.json.{ Json4sOps, Json4sOpsBase }
import com.amazonaws.services.dynamodbv2.document.Item
import org.json4s.JsonAST.JValue

trait DynamoParser extends Parser[Item] {
  def _parse[AST](source: Item)(implicit ops: AstOps[AST, Item]): Option[AST] =
    Json4sOps.parser._parse(source.toJSON())(Json4sOps).asInstanceOf[Option[AST]]
}

trait DynamoRenderer extends Renderer[Item] {
  def _renderCompact[AST](ast: AST, sj: ScalaJackLike[_, _])(implicit ops: AstOps[AST, Item]): Item =
    Item.fromJSON(Json4sOps.renderer._renderCompact(ast, sj)(Json4sOps.asInstanceOf[AstOps[AST, String]]))
}

object DynamoOps extends AstOps[JValue, Item] with Json4sOpsBase with DynamoParser with DynamoRenderer
