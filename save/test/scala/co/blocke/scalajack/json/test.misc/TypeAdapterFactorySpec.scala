package co.blocke.scalajack
package json
package test.misc

import org.scalatest.FunSpec
import org.scalatest.Matchers._

object Factory1 extends TypeAdapterFactory.===.withOneTypeParam[List] {
  def create[E, T <: List[E]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T], ttX: TypeTag[List[E]], ttElement: TypeTag[E]): TypeAdapter[T] = {
    (new TA1[T]()).asInstanceOf[TypeAdapter[T]]
  }
}

object Factory2 extends TypeAdapterFactory.<:<.withOneTypeParam[List] {
  def create[E, T <: List[E]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T], ttX: TypeTag[List[E]], ttElement: TypeTag[E]): TypeAdapter[T] = {
    (new TA2[T]()).asInstanceOf[TypeAdapter[T]]
  }
}

object Factory3 extends TypeAdapterFactory.===.withTwoTypeParams[Map] {
  def create[E1, E2, T <: Map[E1, E2]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T], ttX: TypeTag[Map[E1, E2]], ttElement1: TypeTag[E1], ttElement2: TypeTag[E2]): TypeAdapter[T] = {
    (new TA3[T]()).asInstanceOf[TypeAdapter[T]]
  }
}

object Factory4 extends TypeAdapterFactory.<:<.withTwoTypeParams[Map] {
  def create[E1, E2, T <: Map[E1, E2]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T], ttX: TypeTag[Map[E1, E2]], ttElement1: TypeTag[E1], ttElement2: TypeTag[E2]): TypeAdapter[T] = {
    (new TA4[T]()).asInstanceOf[TypeAdapter[T]]
  }
}

object Factory5 extends TypeAdapterFactory.<:<[String] {
  def create[T <: String](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    (new TA5[T]()).asInstanceOf[TypeAdapter[T]]
  }
}

class TA1[T]() extends TypeAdapter[T]
class TA2[T]() extends TypeAdapter[T]
class TA3[T]() extends TypeAdapter[T]
class TA4[T]() extends TypeAdapter[T]
class TA5[T]() extends TypeAdapter[T]
class TA6[T]() extends TypeAdapter[T]

class TypeAdapterFactorySpec extends FunSpec {
  type Phone = String

  /*
  val context = Context().withFactory(Factory1).withFactory(Factory2).withFactory(Factory3).withFactory(Factory4)

  try {
    println(context.typeAdapterOf[List[Any]].getClass.getName)
    println(context.typeAdapterOf[List[String]].getClass.getName)
    println(context.typeAdapterOf[Map[Any,Any]].getClass.getName)
    println(context.typeAdapterOf[Map[String,Int]].getClass.getName)
  } catch {
    case _ => println("<<< BOOM >>>")
  }

  val ta = new TA6[Boolean]()
  val factory = TypeAdapterFactory.=:=[Boolean](ta)
  val c2 = context.withFactory(factory)
  println(c2.typeAdapterOf[Boolean].getClass.getName)

  val deser = Deserializer.constant(TypeTagged(123.45D))
  val factory2 = TypeAdapterFactory.=:=(deser)
  val c3 = context.withFactory(factory2)
  println(c3.typeAdapterOf[Double].getClass.getName)

  val c4 = context.withFactory(Factory5)
  println(c4.typeAdapterOf[String].getClass.getName)
  println(c4.typeAdapterOf[Phone].getClass.getName)
     */

  val context = Context()
    .withFactory(Factory1)
    .withFactory(Factory2)
    .withFactory(Factory3)
    .withFactory(Factory4)
    .withFactory(Factory5)

  describe("----------------------------\n:  TypeAdapter Type Tests  :\n----------------------------") {
    it("Find with === (one type param)") {
      context.typeAdapterOf[List[Any]].getClass.getName should be("co.blocke.scalajack.json.test.misc.TA1")
    }
    it("Find with <:< (one type param)") {
      context.typeAdapterOf[List[Int]].getClass.getName should be("co.blocke.scalajack.json.test.misc.TA2")
    }
    it("Find with === (two type params)") {
      context.typeAdapterOf[Map[Any, Any]].getClass.getName should be("co.blocke.scalajack.json.test.misc.TA3")
    }
    it("Find with <:< (two type params)") {
      context.typeAdapterOf[Map[String, Int]].getClass.getName should be("co.blocke.scalajack.json.test.misc.TA4")
    }
    it("Show implicit type support with <:<") {
      context.typeAdapterOf[Phone].getClass.getName should be("co.blocke.scalajack.json.test.misc.TA5")
      context.typeAdapterOf[String].getClass.getName should be("co.blocke.scalajack.json.test.misc.TA5")
    }
    it("Register a simple/primitive TypeAdapter") {
      val ta = new TA6[Boolean]()
      val factory = TypeAdapterFactory.=:=[Boolean](ta)
      val c2 = context.withFactory(factory)
      c2.typeAdapterOf[Boolean].getClass.getName should be("co.blocke.scalajack.json.test.misc.TA6")
    }
    it("Create a TypeAdapter from a Deserializer") {
      val deser = Deserializer.constant(TypeTagged(123.45D))
      val factory = TypeAdapterFactory.=:=(deser)
      val c2 = context.withFactory(factory)
      // Creates an anonmyous TypeAdapter
      c2.typeAdapterOf[Double].getClass.getName startsWith ("co.blocke.scalajack.TypeAdapterFactory$") should be(true)
    }
  }
}
