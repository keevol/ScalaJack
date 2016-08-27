package co.blocke.scalajack.flexjson

import co.blocke.scalajack.flexjson.TokenType.TokenType

class TokenReader(override val source: Array[Char],
                  numberOfTokens: Int,
                  tokenTypes: Array[TokenType],
                  tokenOffsets: Array[Int],
                  tokenLengths: Array[Int]) extends Reader {

  override var position = -1

  override def peek: TokenType = tokenTypes(position + 1)

  override def read(expected: TokenType): Unit = {
    position += 1

    val actual = tokenTypes(position)
    if (actual != expected) {
      throw new IllegalStateException(s"Expected token of type $expected, not $actual")
    }
  }

  override def readString(): String = {
    read(expected = TokenType.String)
    tokenText
  }

  override def tokenText: String =
    new String(source, tokenOffsets(position), tokenLengths(position))

  override def read(): TokenType = {
    position += 1
    tokenTypes(position)
  }

  override def tokenOffset: Int = tokenOffsets(position)

  override def tokenLength: Int = tokenLengths(position)

}