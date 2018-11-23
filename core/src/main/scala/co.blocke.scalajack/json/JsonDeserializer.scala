package co.blocke.scalajack
package json

trait JsonDeserializer[IR] extends WireDeserializer[IR, String] {

  this: Ops[IR, String] =>

  private val NumberOfDigitsInMaxLongValue: Int = Long.MaxValue.toString.length

  override def deserialize(path: Path, source: String): DeserializationResult[IR] =
    deserialize(path, source.toCharArray)

  def deserialize(path: Path, source: Array[Char]): DeserializationResult[IR] =
    deserialize(path, source, 0, source.length)

  def deserialize(path: Path, source: Array[Char], offset: Int, length: Int): DeserializationResult[IR] = {
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

          // ---- Unclear if this is needed.  Coverage testing revealed it's never called
          //      in the suite of unit tests.  Leave it here for now... maybe delete later.
          //          case '"' =>
          //            skipChar(expected = '"')
          //            stringBuilder.append('"')
          //            segmentStart += 1
          //            inSlash = false

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
      string
    }

    def readField(): (String, IR) = {
      val startPosition = position
      val key = readJsonValue()
      val endPosition = position

      val keyString =
        key match {
          case IRString(string) => string
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

    def readJsonArray(): IR = {
      val elements = scala.collection.mutable.ListBuffer.empty[IR]
      skipChar(expected = '[')
      skipWhitespace()

      source(position) match {
        case ']' =>
          skipChar(expected = ']')
          skipWhitespace()

        case _ =>
          val initialElement = readJsonValue()
          elements += initialElement

          var readingElements = true
          while (readingElements) {
            skipWhitespace()
            source(position) match {
              case ',' =>
                skipChar(expected = ',')
                skipWhitespace()

                val element = readJsonValue()
                elements += element

              case ']' =>
                skipChar(expected = ']')
                skipWhitespace()
                readingElements = false
            }
          }
      }
      applyArray(elements)
    }

    def readJsonNumber(): IR = {
      val beginIndex = position

      var containsDecimal = false
      var onlyContainsDigits = true

      var readingNumber = true
      while (position < maxPosition && readingNumber) {
        val char = source(position)
        char match {
          case '.' =>
            containsDecimal = true
            onlyContainsDigits = false
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
          applyDecimal(BigDecimal(string))
        } else if (onlyContainsDigits) {
          if (length < NumberOfDigitsInMaxLongValue) {
            applyLong(new String(source, beginIndex, length).toLong)
          } else if (length == NumberOfDigitsInMaxLongValue) {
            // On the border between JLong/JInt
            val string = new String(source, beginIndex, length)
            try {
              applyLong(string.toLong)
            } catch {
              case _: NumberFormatException =>
                applyInt(BigInt(string))
            }
          } else {
            applyInt(BigInt(new String(source, beginIndex, length)))
          }
        } else {
          val string = new String(source, beginIndex, length)
          try {
            applyLong(string.toLong)
          } catch {
            case _: NumberFormatException =>
              applyInt(BigInt(string))
          }
        }

      jsonNumber
    }

    def readJsonObject(): IR = {
      val fields = scala.collection.mutable.ListBuffer.empty[(String, IR)]
      skipChar(expected = '{')
      skipWhitespace()

      source(position) match {
        case '}' =>
          skipChar(expected = '}')
          skipWhitespace()

        case _ =>
          fields += readField()

          var readingFields = true
          while (readingFields) {
            skipWhitespace()
            source(position) match {
              case ',' =>
                skipChar(expected = ',')
                skipWhitespace()

                fields += readField()

              case '}' =>
                skipChar(expected = '}')
                skipWhitespace()
                readingFields = false

              case _ =>
                throw new IllegalArgumentException(s"Malformed JSON: Expected either ',' or '}' at position $position")
            }
          }
      }
      applyObject(fields)
    }

    def readJsonString(): IR = applyString(readString())

    def readJsonValue(): IR =
      source(position) match {
        case '{' =>
          readJsonObject()

        case '[' =>
          readJsonArray()

        case '"' =>
          readJsonString()

        case literalChar if isLiteralChar(literalChar) =>
          readLiteral() match {
            case "null"  => applyNull()
            case "false" => applyBoolean(false)
            case "true"  => applyBoolean(true)
            case s       => applyString(s)
          }

        case numberChar if isNumberChar(numberChar) =>
          readJsonNumber()
      }

    skipWhitespace()
    if (position == maxPosition) {
      DeserializationSuccess(IRNull()) // Nothing to parse!
    } else {
      DeserializationResult(path) { readJsonValue() } match {
        case res @ DeserializationSuccess(_) if (position == maxPosition) => res
        case DeserializationSuccess(ir) =>
          ir match {
            case IRObject(_) =>
              DeserializationFailure(path, ReadError.ExceptionThrown(new IllegalArgumentException(s"Extra, unparsed JSON: '${new String(source.drop(position))}'")))
            case _ =>
              DeserializationSuccess(applyString(source.mkString))
          }
        case other: DeserializationResult[IR] =>
          other
      }
    }
  }

}
