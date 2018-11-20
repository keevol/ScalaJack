package co.blocke.scalajack

import co.blocke.scalajack.Path.{ Element, Field }

object Path {

  case object Root extends Path {
    override def toString: String = "$"
  }

  case object Unknown extends Path {
    override def toString: String = "???"
  }

  case class Field(parent: Path, name: String) extends Path {
    override def toString: String = name(0) match {
      case '{' =>
        val s = name.take(18)
        if (s.endsWith("}"))
          s"""$parent.$s"""
        else
          s"""$parent.$s...}"""
      case '[' =>
        val s = name.take(18)
        if (s.endsWith("]"))
          s"""$parent.$s"""
        else
          s"""$parent.$s...]"""
      case _ if name.contains('.') => s"""$parent."$name""""
      case _                       => s"$parent.$name"
    }
  }

  case class Element(parent: Path, index: Int) extends Path {
    override def toString: String = s"$parent[$index]"
  }

}

sealed trait Path {

  def \(fieldName: String): Field = Field(this, fieldName)

  def \(elementIndex: Int): Element = Element(this, elementIndex)

  def +(next: String) = this match {
    case e: Element => e
    case _          => Field(this, next)
  }

}
