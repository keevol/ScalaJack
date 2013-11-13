package co.blocke.scalajack
package test

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import org.bson.types.ObjectId

class TestSpec extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

	val data = Map("mymap"->Map("hey"->17,"you"->21),"nest"->Two("Nest!",true),"num"->"B","maybe"->Some("wow"),"name"->"Greg","flipflop"->true,"big"->99123986123L,"more"->List(Two("x",false),Two("y",true)),"stuff"->List("a","b"),"num"->Num.C,"age"->46)

	describe("====================\n| -- JSON Tests -- |\n====================") {
		/*
		describe("Basic Render/Read") {
			it( "Serialize simple object to JSON -- all supported data types" ) {
				val a = ScalaJack.poof[One]( data )
				val js = ScalaJack.render(a)
				js should equal( """{"name":"Greg","stuff":["a","b"],"more":[{"foo":"x","bar":false},{"foo":"y","bar":true}],"nest":{"foo":"Nest!","bar":true},"maybe":"wow","mymap":{"hey":17,"you":21},"flipflop":true,"big":99123986123,"num":"C","age":46}""" )
			}
			it( "De-serialize simple object from JSON -- all supported data types" ) {
				val a = ScalaJack.poof[One]( data )
				val js = ScalaJack.render(a)
				js should equal( """{"name":"Greg","stuff":["a","b"],"more":[{"foo":"x","bar":false},{"foo":"y","bar":true}],"nest":{"foo":"Nest!","bar":true},"maybe":"wow","mymap":{"hey":17,"you":21},"flipflop":true,"big":99123986123,"num":"C","age":46}""" )
				val b = ScalaJack.read[One](js)
				b should equal( a )
			}
			it( "Handle empty Lists & Maps") {
				val four = Four(List[String](), Map[String,Int]())
				val js = ScalaJack.render(four)
				js should equal( """{"stuff":[],"things":{}}""" )
				ScalaJack.read[Four](js) should equal( four )
			}
			it( "Naked Lists of string" ) {
				val stuff = List( "a","b","c" )
				val js = ScalaJack.renderList(stuff)
				js should equal( """["a","b","c"]""" )
				ScalaJack.readList[String](js) should equal( stuff )
			}
			it( "Naked Lists of objects" ) {
				val stuff = List( Three("three",Num.A,Wow1("foo",17)), Three("four",Num.B,Wow1("bar",18)) )
				val js = ScalaJack.renderList(stuff)
				js should equal( """[{"name":"three","two":"A","pp":{"_hint":"co.blocke.scalajack.test.Wow1","a":"foo","b":17}},{"name":"four","two":"B","pp":{"_hint":"co.blocke.scalajack.test.Wow1","a":"bar","b":18}}]""" )
				ScalaJack.readList[Three](js) should equal( stuff )
			}
		}
		describe("Trait Support") {
			it( "Traits with subclasses" ) {
				val t = Three("three",Num.A,Wow1("foo",17))
				val js2 = ScalaJack.render(t)
				js2 should equal( """{"name":"three","two":"A","pp":{"_hint":"co.blocke.scalajack.test.Wow1","a":"foo","b":17}}""" )
				val u = ScalaJack.read[Three](js2)
				u should equal( t )
			}
			it( "Support changing type hint" ) {
				val t = Three("three",Num.A,Wow1("foo",17))
				val js2 = ScalaJack.render(t,false,"hey")
				js2 should equal( """{"name":"three","two":"A","pp":{"hey":"co.blocke.scalajack.test.Wow1","a":"foo","b":17}}""" )
				val u = ScalaJack.read[Three](js2)
				u should equal( t )
			}
			it("Top-level trait") {
				val w = Wow1("hey",99)
				val js = ScalaJack.render[Pop](w)
				js should equal("""{"_hint":"co.blocke.scalajack.test.Wow1","a":"hey","b":99}""")
				ScalaJack.read[Pop](js) should equal(w)
			}
		}
		describe("Value Classes") {
			describe("Without custom JSON support") {
				it( "Simple value class support" ) {
					val stuff = ValSupport("foo", new Wrapper(42), false)
					val js = ScalaJack.render(stuff)
					js should equal( """{"name":"foo","wrap":42,"more":false}""" )
					ScalaJack.read[ValSupport](js) should equal( stuff )
				}
				it( "List of value class without custom JSON support" ) {
					val stuff = ListValSupport("bar", List(new Wrapper(99),new Wrapper(100)), true)
					val js = ScalaJack.render(stuff)
					js should equal("""{"name":"bar","wrap":[99,100],"more":true}""")
					ScalaJack.read[ListValSupport](js) should equal( stuff )
				}
				it( "Option of value class without custom JSON support" ) {
					val stuff = OptValSupport("hey", Some(new Wrapper(2)))
					val stuff2 = OptValSupport("hey", None)
					val js1 = ScalaJack.render(stuff)
					val js2 = ScalaJack.render(stuff2)
					js1 should equal("""{"name":"hey","wrap":2}""")
					js2 should equal("""{"name":"hey"}""")
					ScalaJack.read[OptValSupport](js1) should equal( stuff )
					ScalaJack.read[OptValSupport](js2) should equal( stuff2 )
				}
				it( "Map of value class without custom JSON support" ) {
					val stuff = MapValSupport("hey", Map("blah"->new Wrapper(2),"wow"->new Wrapper(3)))
					val js2 = ScalaJack.render(stuff)
					js2 should equal("""{"name":"hey","wrap":{"blah":2,"wow":3}}""")
					ScalaJack.read[MapValSupport](js2) should equal( stuff )
				}
			}
			describe("With custom JSON support") {
				it( "Simple value custom JSON support for Value class" ) {
					val stuff = ValSupport("foo", new Wrapper(99), true)
					val js = ScalaJack.render(stuff,true)
					js should equal("""{"name":"foo","wrap":{"num":99,"hey":"you"},"more":true}""")
					ScalaJack.read[ValSupport](js,true) should equal( stuff )
				}
				it( "List of value class with custom JSON support" ) {
					val stuff = ListValSupport("bar", List(new Wrapper(99),new Wrapper(100)), true)
					val js = ScalaJack.render(stuff,true)
					js should equal("""{"name":"bar","wrap":[{"num":99,"hey":"you"},{"num":100,"hey":"you"}],"more":true}""")
					ScalaJack.read[ListValSupport](js,true) should equal( stuff )
				}
				it( "Option of value class with custom JSON support" ) {
					val stuff = OptValSupport("hey", Some(new Wrapper(2)))
					val stuff2 = OptValSupport("hey", None)
					val js1 = ScalaJack.render(stuff,true)
					val js2 = ScalaJack.render(stuff2,true)
					js1 should equal("""{"name":"hey","wrap":{"num":2,"hey":"you"}}""")
					js2 should equal("""{"name":"hey"}""")
					ScalaJack.read[OptValSupport](js1,true) should equal( stuff )
					ScalaJack.read[OptValSupport](js2,true) should equal( stuff2 )
				}
				it( "Map of value class without custom JSON support" ) {
					val stuff = MapValSupport("hey", Map("blah"->new Wrapper(2),"wow"->new Wrapper(3)))
					val js1 = ScalaJack.render(stuff,true)
					js1 should equal("""{"name":"hey","wrap":{"blah":{"num":2,"hey":"you"},"wow":{"num":3,"hey":"you"}}}""")
					ScalaJack.read[MapValSupport](js1,true) should equal( stuff )
				}
			}
		}
		describe("Nested Constructs") {
			describe("With Lists") {
				it( "List of lists of case classes" ) {
					val ln = ListList("Fred", List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4))))
					val js = ScalaJack.render(ln)
					js should equal( """{"name":"Fred","stuff":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}""" )
					ScalaJack.read[ListList](js) should equal( ln )
				}
				it( "List of lists of lists of case classes" ) {
					val ln = ListListList("Fred",
						List(
							List(
								List(
									Animal("mouse", 4),
									Animal("bug", 6)),
								List(
									Animal("whale", 0),
									Animal("elephant", 4))),
							List(
								List(
									Animal("millipede", 1000),
									Animal("slug", 0)),
								List(
									Animal("bird", 2),
									Animal("tiger", 4)))))
					val js = ScalaJack.render(ln)
					js should equal( """{"name":"Fred","stuff":[[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]],[[{"name":"millipede","legs":1000},{"name":"slug","legs":0}],[{"name":"bird","legs":2},{"name":"tiger","legs":4}]]]}""" )
					ScalaJack.read[ListListList](js) should equal( ln ) 
				}
				// NOTE: If your list has a None it it, this will be lost upon re-marshal from JSON as JSON has no representation
				//       for a None (it's simply missing from the list).
				it( "List of option of case classes" ) {
					val lop = ListOpt("Jenny", List(Some(Animal("mouse", 4)), None, Some(Animal("whale", 0))))
					val js = ScalaJack.render(lop)
					js should equal( """{"name":"Jenny","stuff":[{"name":"mouse","legs":4},{"name":"whale","legs":0}]}""" )
					ScalaJack.read[ListOpt](js) should equal( lop.copy(stuff = lop.stuff.filter(_.isDefined) ) )
				}
				it( "List of map of case classes" ) {
					val lm = ListMap("Jenny", List(Map("a" -> Animal("mouse", 4)), Map("b" -> Animal("whale", 0))))
					val js = ScalaJack.render(lm)
					js should equal( """{"name":"Jenny","stuff":[{"a":{"name":"mouse","legs":4}},{"b":{"name":"whale","legs":0}}]}""" )
					ScalaJack.read[ListMap](js) should equal( lm )
				}
			}
			describe("With Option") {
				it( "Option of list of case classes" ) {
					val oln = OpList("Wow", Some(List(Animal("mouse", 4), Animal("bug", 6))))
					val js = ScalaJack.render(oln)
					js should equal( """{"name":"Wow","opList":[{"name":"mouse","legs":4},{"name":"bug","legs":6}]}""" )
					ScalaJack.read[OpList](js) should equal( oln )
				}
				it( "Option of nested list of case classes" ) {
					val oln = OpListList("Yay", Some(List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4)))))
					val js = ScalaJack.render(oln)
					js should equal( """{"name":"Yay","opListList":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}""" )
					ScalaJack.read[OpListList](js) should equal( oln )
				}
				it( "Option of map of case classes" ) {
					val om = OpMap("Wow", Some(Map("hello" -> (Animal("mouse", 4)))))
					val js = ScalaJack.render(om)
					js should equal( """{"name":"Wow","opMap":{"hello":{"name":"mouse","legs":4}}}""" )
					ScalaJack.read[OpMap](js) should equal( om )
					val om2 = OpMap("Wow", None)
					val js2 = ScalaJack.render(om2)
					js2 should equal( """{"name":"Wow"}""" )
					ScalaJack.read[OpMap](js2) should equal( om2 )
				}
				it( "Nested Option of case classes" ) {
					val oop = OpOp("Oops", Some(Some(Animal("mouse", 4))))
					val js = ScalaJack.render(oop)
					js should equal( """{"name":"Oops","opts":{"name":"mouse","legs":4}}""" )
					ScalaJack.read[OpOp](js) should equal( oop )
					val oop2 = OpOp("Oops", None)
					val js2 = ScalaJack.render(oop2)
					js2 should equal( """{"name":"Oops"}""" )
					ScalaJack.read[OpOp](js2) should equal( oop2 )
				}
			}
			describe("With Map") {
				it( "Map of list of case classes" ) {
					val mln = MapList("Bob", Map("Mike" -> List(Animal("mouse", 4), Animal("bug", 6)), "Sally" -> List(Animal("whale", 0), Animal("elephant", 4))))
					val js = ScalaJack.render(mln)
					js should equal( """{"name":"Bob","mapList":{"Mike":[{"name":"mouse","legs":4},{"name":"bug","legs":6}],"Sally":[{"name":"whale","legs":0},{"name":"elephant","legs":4}]}}""" )
					ScalaJack.read[MapList](js) should equal( mln )
				}
				it( "Map of nested lists of case classes" ) {
					val mln = MapListList("Bob", Map("Everyone" -> List(List(Animal("mouse", 4), Animal("bug", 6)), List(Animal("whale", 0), Animal("elephant", 4)))))
					val js = ScalaJack.render(mln)
					js should equal( """{"name":"Bob","mapList":{"Everyone":[[{"name":"mouse","legs":4},{"name":"bug","legs":6}],[{"name":"whale","legs":0},{"name":"elephant","legs":4}]]}}""" )
					ScalaJack.read[MapListList](js) should equal( mln )
				}
				it( "Map of option of case classes" ) {
					val a: Option[Animal] = None
					val mln = MapOpt("Bob", Map("things" -> Some(Animal("mouse", 4)), "otherthings" -> a))
					val js = ScalaJack.render(mln)
					js should equal( """{"name":"Bob","mapOpt":{"things":{"name":"mouse","legs":4}}}""" )
					ScalaJack.read[MapOpt](js) should equal( mln.copy(mapOpt = mln.mapOpt.filter({ case (k, v) => v.isDefined })) )
				}
				it( "Map of map of case classes" ) {
					val mm = MapMap("Bob", Map("things" -> Map("a" -> Animal("mouse", 4), "b" -> Animal("horse", 4)), "stuff" -> Map("c" -> Animal("sloth", 2))))
					val js = ScalaJack.render(mm)
					js should equal( """{"name":"Bob","mapmap":{"things":{"a":{"name":"mouse","legs":4},"b":{"name":"horse","legs":4}},"stuff":{"c":{"name":"sloth","legs":2}}}}""" )
					ScalaJack.read[MapMap](js) should equal( mm )
				}
			}
		}
		describe("MongoDB/Casbah Support") {
			it( "MongoKey Annotation (_id field generation) - single key" ) {
				val five = Five("Fred",Two("blah",true))
				val dbo = ScalaJack.renderDB(five)
				dbo.toString should equal( """{ "_id" : "Fred" , "two" : { "foo" : "blah" , "bar" : true}}""" )
				ScalaJack.readDB[Five](dbo) should equal( five )
			}
			it( "MongoKey Annotation (_id field generation) - compound key" ) {
				val six = Six("Fred",12,Two("blah",true))
				val dbo = ScalaJack.renderDB(six)
				dbo.toString should equal( """{ "_id" : { "name" : "Fred" , "num" : 12} , "two" : { "foo" : "blah" , "bar" : true}}""" )
				ScalaJack.readDB[Six](dbo) should equal( six )
			}
			it("ObjectId support") {
				val oid = new ObjectId
				val seven = Seven(oid,Two("blah",true))
				val dbo = ScalaJack.renderDB(seven)
				dbo.toString should equal( """{ "_id" : { "$oid" : """"+oid+""""} , "two" : { "foo" : "blah" , "bar" : true}}""" )
				ScalaJack.readDB[Seven](dbo) should equal( seven )
			}
		}
		*/
		describe("Parameterized Class Support") {
			/*
			it("Basic parameterized case class") {
				val w = Wrap("number",true,15)
				val w2 = Wrap("number",true,"wow")
				val js = ScalaJack.render(w)
				val js2 = ScalaJack.render(w2)
				val db = ScalaJack.renderDB(w)
				val db2 = ScalaJack.renderDB(w2)
				js should equal("""{"name":"number","data":true,"stuff":15}""")
				js2 should equal("""{"name":"number","data":true,"stuff":"wow"}""")
				db.toString should equal("""{ "name" : "number" , "data" : true , "stuff" : 15}""")
				db2.toString should equal("""{ "name" : "number" , "data" : true , "stuff" : "wow"}""")
				ScalaJack.read[Wrap[Boolean,Int]](js) should equal(w)
				ScalaJack.read[Wrap[Boolean,String]](js2) should equal(w2)
				ScalaJack.readDB[Wrap[Boolean,Int]](db) should equal(w)
				ScalaJack.readDB[Wrap[Boolean,String]](db2) should equal(w2)
			}
			it("Basic parameterized case class having a non-parameter case clase as a field member") {
				val w = Truck(false, Two("z",true))
				val js = ScalaJack.render(w)
				js should equal("""{"s":false,"t":{"foo":"z","bar":true}}""")
				ScalaJack.read[Truck[Boolean]](js) should equal(w)
			}
			it("Basic parameterized case class having case class parameter") {
				val w = Wrap("number",true,Two("a",false))
				val js = ScalaJack.render(w)
				js should equal("""{"name":"number","data":true,"stuff":{"foo":"a","bar":false}}""")
				val db = ScalaJack.renderDB(w)
				db.toString should equal("""{ "name" : "number" , "data" : true , "stuff" : { "foo" : "a" , "bar" : false}}""")
				ScalaJack.read[Wrap[Boolean,Two]](js) should equal(w)
				ScalaJack.readDB[Wrap[Boolean,Two]](db) should equal(w)
			}
			*/
			it("Basic parameterized case class having case class parameter, itself with a parameter") {
				val w = Carry("Bob", Wrap("Mary",35,"Available"))
				val js = ScalaJack.render(w)
				println(js)
			}
			it("Basic parameterized case class having trait parameter") {
				(pending)
			}
			it("Embedded parameterized case class") {
				// val c = Carry("Wow", Wrap("hello",5,"Boo"))
				// val js = ScalaJack.render(c)
				// println(js)
				(pending)
			}
			it("Container of parameterized case class") {
				(pending)
			}
			it("Basic parameterized trait") {
				(pending)
			}
			it("Basic parameterized trait having case class parameter") {
				(pending)
			}
			it("Basic parameterized trait having trait parameter") {
				(pending)
			}
			it("Embedded parameterized trait") {
				(pending)
			}
			it("Container of parameterized trait") {
				(pending)
			}
		}
	}
}