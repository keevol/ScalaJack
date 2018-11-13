package co.blocke.scalajack

case class Person(name: String, age: Int, isOk: Boolean)

object RunMe extends App {

  val sj = ScalaJack()
  val p = Person("Greg", 52, true)
  val js = sj.render(p)
  println(js)

  println(sj.read[Person](js))
}

