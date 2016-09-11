package co.blocke.scalajack.flexjson

object StructureType extends Enumeration {
  type StructureType = Value
  val Object, Array = Value
}

object ValueType extends Enumeration {
  type ValueType = Value
  val String, Number, Boolean, Identifier, Object, Array, Unreadable, Raw, Nothing = Value
}

object MemberPart extends Enumeration {
  type MemberPart = Value
  val MemberName, MemberValue = Value
}

import co.blocke.scalajack.RenderException
import co.blocke.scalajack.flexjson.StructureType.StructureType
import co.blocke.scalajack.flexjson.ValueType.ValueType
import co.blocke.scalajack.flexjson.MemberPart.MemberPart

class StringJsonWriter(canonical: Boolean = true) extends Writer {

  sealed trait Structure {

    val structureType: StructureType

    def parent: Structure

    def beginChildValue(nestedValueType: ValueType): Unit

    def endChildValue(nestedValueType: ValueType): Unit

    def end(expectedStructureType: StructureType): Structure = {
      val actualStructureType = structureType
      if (expectedStructureType != actualStructureType) {
        throw new RuntimeException(s"Attempting to end an $actualStructureType structure when a $expectedStructureType is currently open")
      }

      parent
    }

  }

  object RootStructure extends Structure {

    override val structureType: StructureType = null

    override def parent: Structure = ???

    override def beginChildValue(nestedValueType: ValueType): Unit = {}

    override def endChildValue(nestedValueType: ValueType): Unit = {}

  }

  class ObjectStructure(override val parent: Structure) extends Structure {

    var numberOfMembersWrittenSoFar: Int = 0
    var nextMemberPartToBeWritten: MemberPart = MemberPart.MemberName
    var memberPartCurrentlyBeingWritten: MemberPart = null
    var builderLengthBeforeMemberNameWritten: Int = 0

    override val structureType = StructureType.Object

    override def beginChildValue(childValueType: ValueType): Unit = {
      nextMemberPartToBeWritten match {
        case MemberPart.MemberName ⇒

          if (canonical && childValueType != ValueType.String) {
            throw new RenderException(s"Member names must be of type ${TokenType.String}, not $childValueType")
          }

          builderLengthBeforeMemberNameWritten = builder.length // Just in case the value is Nothing
          if (numberOfMembersWrittenSoFar > 0) {
            writeValueSeparator()
          }

          memberPartCurrentlyBeingWritten = MemberPart.MemberName
          nextMemberPartToBeWritten = MemberPart.MemberValue

        case MemberPart.MemberValue ⇒
          writeNameSeparator()
          if (childValueType == ValueType.Nothing) {
            builder.length = builderLengthBeforeMemberNameWritten
          }

          memberPartCurrentlyBeingWritten = MemberPart.MemberValue
          nextMemberPartToBeWritten = MemberPart.MemberName
      }
    }

    override def endChildValue(childValueType: ValueType): Unit = {
      if (memberPartCurrentlyBeingWritten == MemberPart.MemberValue && childValueType != ValueType.Nothing) {
        numberOfMembersWrittenSoFar += 1
      }

      memberPartCurrentlyBeingWritten = null
    }

  }

  class ArrayStructure(override val parent: Structure) extends Structure {

    var numberOfElementsWrittenSoFar: Int = 0
    var builderLengthBeforeElementWritten: Int = 0

    override val structureType = StructureType.Array

    override def beginChildValue(childValueType: ValueType): Unit = {
      builderLengthBeforeElementWritten = builder.length
      if (numberOfElementsWrittenSoFar > 0) {
        writeValueSeparator()
      }
      if (childValueType == ValueType.Nothing) {
        builder.length = builderLengthBeforeElementWritten
      }
    }

    override def endChildValue(childValueType: ValueType): Unit = {
      if (childValueType != ValueType.Nothing) {
        numberOfElementsWrittenSoFar += 1
      }
    }

  }

  val builder = new StringBuilder
  var structure: Structure = RootStructure

  def jsonString: String = builder.toString

  override def beginObject(): Unit = {
    structure.beginChildValue(ValueType.Object)
    structure = new ObjectStructure(structure)
    builder.append("{")
  }

  override def endObject(): Unit = {
    builder.append("}")
    structure = structure.end(expectedStructureType = StructureType.Object)
    structure.endChildValue(ValueType.Object)
  }

  override def beginArray(): Unit = {
    structure.beginChildValue(ValueType.Array)
    structure = new ArrayStructure(structure)
    builder.append("[")
  }

  override def endArray(): Unit = {
    builder.append("]")
    structure = structure.end(expectedStructureType = StructureType.Array)
    structure.endChildValue(ValueType.Array)
  }

  override def writeRawValue(source: Array[Char], offset: Int, length: Int): Unit = {
    structure.beginChildValue(ValueType.Raw)
    builder.appendAll(source, offset, length)
    structure.endChildValue(ValueType.Raw)
  }

  override def writeNothing(): Unit = {
    structure.beginChildValue(ValueType.Nothing)
    structure.endChildValue(ValueType.Nothing)
  }

  override def writeString(string: String): Unit = {
    structure.beginChildValue(ValueType.String)
    builder.append('"')

    var i = 0
    val length = string.length

    var startOfUnescapedCharacters = 0

    while (i < length) {
      string.charAt(i) match {
        case '"' ⇒
          if (i > startOfUnescapedCharacters) builder.append(string.substring(startOfUnescapedCharacters, i))
          builder.append("""\"""")
          startOfUnescapedCharacters = i

        case '\\' ⇒
          if (i > startOfUnescapedCharacters) builder.append(string.substring(startOfUnescapedCharacters, i))
          builder.append("""\\""")
          startOfUnescapedCharacters = i

        case '/' ⇒
          if (i > startOfUnescapedCharacters) builder.append(string.substring(startOfUnescapedCharacters, i))
          builder.append("""\/""")
          startOfUnescapedCharacters = i

        case '\b' ⇒
          if (i > startOfUnescapedCharacters) builder.append(string.substring(startOfUnescapedCharacters, i))
          builder.append("""\b""")
          startOfUnescapedCharacters = i

        case '\f' ⇒
          if (i > startOfUnescapedCharacters) builder.append(string.substring(startOfUnescapedCharacters, i))
          builder.append("""\f""")
          startOfUnescapedCharacters = i

        case '\n' ⇒
          if (i > startOfUnescapedCharacters) builder.append(string.substring(startOfUnescapedCharacters, i))
          builder.append("""\n""")
          startOfUnescapedCharacters = i

        case '\r' ⇒
          if (i > startOfUnescapedCharacters) builder.append(string.substring(startOfUnescapedCharacters, i))
          builder.append("""\r""")
          startOfUnescapedCharacters = i

        case '\t' ⇒
          if (i > startOfUnescapedCharacters) builder.append(string.substring(startOfUnescapedCharacters, i))
          builder.append("""\t""")
          startOfUnescapedCharacters = i

        case ch ⇒
      }

      i += 1
    }

    if (i > startOfUnescapedCharacters) {
      if (startOfUnescapedCharacters == 0) {
        builder.append(string)
      } else {
        builder.append(string.substring(startOfUnescapedCharacters, i))
      }
    }

    builder.append('"') // TODO escape values
    structure.endChildValue(ValueType.String)
  }

  override def writeInt(value: Int): Unit = {
    structure.beginChildValue(ValueType.Number)
    builder.append(value)
    structure.endChildValue(ValueType.Number)
  }

  def writeNameSeparator(): Unit =
    builder.append(":")

  def writeValueSeparator(): Unit = {
    builder.append(",")
  }

  override def writeFalse(): Unit = {
    structure.beginChildValue(ValueType.Identifier)
    builder.append("false")
    structure.endChildValue(ValueType.Identifier)
  }

  override def writeTrue(): Unit = {
    structure.beginChildValue(ValueType.Identifier)
    builder.append("true")
    structure.endChildValue(ValueType.Identifier)
  }

  override def writeNull(): Unit = {
    structure.beginChildValue(ValueType.Identifier)
    builder.append("null")
    structure.endChildValue(ValueType.Identifier)
  }

  override def writeFloat(value: Float): Unit = {
    structure.beginChildValue(ValueType.Number)
    builder.append(value)
    structure.endChildValue(ValueType.Number)
  }

  override def writeDouble(value: Double): Unit = {
    structure.beginChildValue(ValueType.Number)
    builder.append(value)
    structure.endChildValue(ValueType.Number)
  }

  override def writeLong(value: Long): Unit = {
    structure.beginChildValue(ValueType.Number)
    builder.append(value)
    structure.endChildValue(ValueType.Number)
  }

  override def writeChar(value: Char): Unit = {
    structure.beginChildValue(ValueType.String)
    builder.append('"').append(value).append('"')
    structure.endChildValue(ValueType.String)
  }

  override def writeByte(value: Byte): Unit = {
    structure.beginChildValue(ValueType.Number)
    builder.append(value)
    structure.endChildValue(ValueType.Number)
  }

  override def writeShort(value: Short): Unit = {
    structure.beginChildValue(ValueType.Number)
    builder.append(value)
    structure.endChildValue(ValueType.Number)
  }

  override def writeBoolean(value: Boolean): Unit = {
    structure.beginChildValue(ValueType.Boolean)
    builder.append(value)
    structure.endChildValue(ValueType.Boolean)
  }

}
