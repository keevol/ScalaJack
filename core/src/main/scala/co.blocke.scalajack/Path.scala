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
    override def toString: String = {
      if (name.startsWith("{") || name.startsWith("["))
        s"""$parent"""
      else if (name.contains('.'))
        s"""$parent."$name""""
      else
        s"$parent.$name"
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
    case _: Field   => Field(this, next)
    case e: Element => e
  }

}
