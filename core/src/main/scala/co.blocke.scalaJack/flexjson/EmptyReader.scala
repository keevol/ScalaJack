package co.blocke.scalajack.flexjson
import co.blocke.scalajack.flexjson.TokenType.TokenType

object EmptyReader extends Reader {

  override def peek: TokenType = TokenType.Nothing

  override def read(expected: TokenType): Unit = ???

  override def readString(): String = ???
  override def readIdentifier(): String = ???

  override def tokenText: String = ???

  override var position: Int = _

  override def read(): TokenType = ???

  override def source: Array[Char] = ???

  override def tokenOffset: Int = ???

  override def tokenLength: Int = ???

}
