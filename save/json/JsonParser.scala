package co.blocke.scalajack
package json

trait JsonParser extends Parser[String] {

  private val NumberOfDigitsInMaxLongValue: Int = Long.MaxValue.toString.length

  def _parse[AST](source: String)(implicit ops: AstOps[AST, String]): Option[AST] =
    _parse[AST](source.toCharArray)

  def _parse[AST](source: Array[Char])(implicit ops: AstOps[AST, String]): Option[AST] =
    _parse[AST](source, 0, source.length)

  def _parse[AST](source: Array[Char], offset: Int, length: Int)(implicit ops: AstOps[AST, String]): Option[AST] = {
    var position = offset
    val maxPosition = offset + length

    @inline def isWhitespace(char: Char): Boolean =
      char match {
        case ' ' | '\r' | '\t' | '\n' => true
        case _                        => false
      }

    @inline def skipWhitespace(): Unit = {
      while (position < maxPosition && isWhitespace(source(position))) {
        position += 1
      }
    }

    @inline def skipChar(expected: Char): Unit = {
      val actual = source(position)
      if (actual == expected) {
        position += 1
      } else {
        throw new IllegalArgumentException(s"Skipped '$actual', not '$expected'")
      }
    }

    @inline def isLiteralChar(char: Char): Boolean =
      ('a' <= char && char < 'z') || ('A' <= char && char <= 'Z') || char == '_'

    @inline def isDigitChar(char: Char): Boolean =
      '0' <= char && char <= '9'

    @inline def isNumberChar(char: Char): Boolean =
      isDigitChar(char) || (char == '-') || (char == '.') || (char == 'e') || (char == 'E') || (char == '-') || (char == '+')

    def readLiteral(): String = {
      val beginIndex = position
      while (position < maxPosition && isLiteralChar(source(position))) {
        position += 1
      }
      val endIndex = position
      new String(source, beginIndex, endIndex - beginIndex)
    }

    def readString(): String = {
      skipChar(expected = '"')
      val beginIndex = position

      var segmentStart = position
      var stringBuilder: StringBuilder = null

      var reading = true
      var inSlash = false
      while (reading) {
        val char = source(position)
        char match {
          case '"' if (!inSlash) =>
            reading = false

          case '"' =>
            skipChar(expected = '"')
            stringBuilder.append('"')
            segmentStart += 1
            inSlash = false

          case '\\' =>
            if (stringBuilder eq null) stringBuilder = new StringBuilder
            stringBuilder.appendAll(source, segmentStart, position - segmentStart)
            skipChar(expected = '\\')
            inSlash = false

            source(position) match {
              case '"' =>
                skipChar(expected = '"')
                stringBuilder.append('"')

              case '\\' =>
                skipChar(expected = '\\')
                stringBuilder.append('\\')
                inSlash = true

              case '/' =>
                skipChar(expected = '/')
                stringBuilder.append('/')

              case 'b' =>
                skipChar(expected = 'b')
                stringBuilder.append('\b')

              case 'f' =>
                skipChar(expected = 'f')
                stringBuilder.append('\f')

              case 'n' =>
                skipChar(expected = 'n')
                stringBuilder.append('\n')

              case 'r' =>
                skipChar(expected = 'r')
                stringBuilder.append('\r')

              case 't' =>
                skipChar(expected = 't')
                stringBuilder.append('\t')

              case 'u' =>
                skipChar(expected = 'u')

                val hex = new String(source, position, 4)
                stringBuilder.append(Integer.parseInt(hex, 16).toChar)
                position += 4
            }

            segmentStart = position

          case _ =>
            position += 1
        }
      }

      val endIndex = position
      skipChar(expected = '"')

      val string =
        if (stringBuilder eq null) {
          new String(source, beginIndex, endIndex - beginIndex)
        } else {
          stringBuilder.appendAll(source, segmentStart, endIndex - segmentStart)
          stringBuilder.result()
        }

      //      println(string + "\n-----------------------")
      string
    }

    def readField(): (String, AST) = {
      val startPosition = position
      val key = readJsonValue()
      val endPosition = position

      val keyString =
        key match {
          case AstString(string) => string
          case _ =>
            // Non-standard JSON key
            new String(source, startPosition, endPosition - startPosition)
        }

      skipWhitespace()

      skipChar(expected = ':')
      skipWhitespace()

      val value = readJsonValue()
      (keyString, value)
    }

    def readJsonArray(): AST =
      ops applyArray { appendElement =>
        skipChar(expected = '[')
        skipWhitespace()

        source(position) match {
          case ']' =>
            skipChar(expected = ']')
            skipWhitespace()

          case _ =>
            val initialElement = readJsonValue()
            appendElement(initialElement)

            var readingElements = true
            while (readingElements) {
              skipWhitespace()
              source(position) match {
                case ',' =>
                  skipChar(expected = ',')
                  skipWhitespace()

                  val element = readJsonValue()
                  appendElement(element)

                case ']' =>
                  skipChar(expected = ']')
                  skipWhitespace()
                  readingElements = false
              }
            }
        }
      }

    def readJsonNumber(): AST = {
      val beginIndex = position

      var containsDecimal = false
      var onlyContainsDigits = true

      var readingNumber = true
      while (position < maxPosition && readingNumber) {
        val char = source(position)
        char match {
          case '.' =>
            containsDecimal = true
            if (char != '-') {
              onlyContainsDigits = false
            }
            position += 1
          case _ if (isDigitChar(char)) =>
            position += 1
          case _ if (isNumberChar(char)) =>
            position += 1
            onlyContainsDigits = false
          case _ =>
            readingNumber = false
        }
      }

      val endIndex = position
      val length = endIndex - beginIndex

      val jsonNumber =
        if (containsDecimal) {
          val string = new String(source, beginIndex, endIndex - beginIndex)
          ops.applyDecimal(BigDecimal(string))
        } else if (onlyContainsDigits) {
          if (length < NumberOfDigitsInMaxLongValue) {
            ops.applyLong(new String(source, beginIndex, length).toLong)
          } else if (length == NumberOfDigitsInMaxLongValue) {
            // On the border between JLong/JInt
            val string = new String(source, beginIndex, length)
            try {
              ops.applyLong(string.toLong)
            } catch {
              case _: NumberFormatException =>
                ops.applyInt(BigInt(string))
            }
          } else {
            ops.applyInt(BigInt(new String(source, beginIndex, length)))
          }
        } else {
          val string = new String(source, beginIndex, length)
          try {
            ops.applyLong(string.toLong)
          } catch {
            case _: NumberFormatException =>
              ops.applyInt(BigInt(string))
          }
        }

      jsonNumber
    }

    def readJsonObject(): AST =
      ops applyObject { appendField =>
        skipChar(expected = '{')
        skipWhitespace()

        source(position) match {
          case '}' =>
            skipChar(expected = '}')
            skipWhitespace()

          case _ =>
            val (initialFieldName, initialFieldValue) = readField()
            appendField(initialFieldName, initialFieldValue)

            var readingFields = true
            while (readingFields) {
              skipWhitespace()
              source(position) match {
                case ',' =>
                  skipChar(expected = ',')
                  skipWhitespace()

                  val (fieldName, fieldValue) = readField()
                  appendField(fieldName, fieldValue)

                case '}' =>
                  skipChar(expected = '}')
                  skipWhitespace()
                  readingFields = false

                case _ =>
                  throw new IllegalArgumentException(s"Malformed JSON: Expected either ',' or '}' at position $position")
              }
            }
        }
      }

    def readJsonString(): AST =
      ops.applyString(readString())

    def readJsonValue(): AST = {
      source(position) match {
        case '{' =>
          readJsonObject()

        case '[' =>
          readJsonArray()

        case '"' =>
          readJsonString()

        case literalChar if isLiteralChar(literalChar) =>
          readLiteral() match {
            case "null"  => ops.applyNull()
            case "false" => ops.applyBoolean(false)
            case "true"  => ops.applyBoolean(true)
            case s       => ops.applyString(s)
          }

        case numberChar if isNumberChar(numberChar) =>
          readJsonNumber()
      }
    }

    skipWhitespace()
    if (position == maxPosition) {
      None
    } else {
      val parsedOutcome = Some(readJsonValue())
      if (position != maxPosition) {
        parsedOutcome match {
          case Some(x) if (ops.isObject(x) || ops.isArray(x)) =>
            // Extra chars at the end of a JSON array or object is a parse error
            throw new IllegalArgumentException(s"Extra, unparsed JSON: '${new String(source.drop(position))}'")
          case Some(_) =>
            // Extra chars at the end of a primitive JSON type -> treat the whole thing as a String, e.g. 0xlr
            // will be detected as a number(0) with "extra" chars 'xlr'.  Gives false result of a Long(0).  Actual
            // result should be String(0xlr).
            Some(ops.applyString(source.mkString))
          case a => a
        }
      } else
        parsedOutcome
    }
  }

}