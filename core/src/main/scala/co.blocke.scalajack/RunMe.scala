package co.blocke.scalajack

import org.json4s._

object RunMe extends App {

  val j = JArray(List(JString("a"), JString("b")))
  println(j.toString())
}

