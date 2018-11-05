## Any Support

Scala has the wonderful concept of Any.  ScalaJack supports Any but you should be aware of its special needs and limitations.

A value of type Any means ScalaJack has no specific idea what the type should be, and must therefore infer the type.  This is necessarily an imperfect process but we can describe the process it uses here.

Let's use a simple sample:

```scala
package com.me

case class Amorphous(
  thing: Any
  )
case class Small(num:Int)

val all = List(
  Amorphous(true),
  Amorphous("blather"),
  Amorphous(1.234),
  Amorphous(List(1,2,3)),
  Amorphous(Map("a"->1,"b"->2)),
  Amorphous(null),
  Amorphous(Small(99))
  )

sj.render(all)
```

This renders:

```JSON
[
  {"thing":true},
  {"thing":"blather"},
  {"thing":1.234},
  {"thing":[1,2,3]},
  {"thing":{"a":1,"b":2}},
  {"thing":null},
  {"thing":{"_hint":"com.me.Small","num":99}}
]
```
So far, so good, right?  It gets a bit more complicated... read on.

#### Classes and Maps
You can see from the sample above that when a Any-typed value is populated with an object, it is rendered like a trait, with its type hint (only the default _hint supported here for now).  This is so ScalaJack knows what class to materialize upon reading this JSON.

Without a type hint, the JSON object will be inferred to be a key/value Map in Scala.

**Note:** Option[] values cannot be supported as an Any value.  Rendering Some(thing) would always be read as thing.  ScalaJack would never be able to infer Some(thing) vs thing from JSON.

#### Numbers
When reading a numerical value, ScalaJack must infer what kind of numerical type to use.  There's no right/wrong answer here, so ScalaJack uses a simple fitting mechanism.  The fittings are shown in the table below, but one important thing to keep in mind: If you render an Any numerical value and read it back in, the value read in may be a different type than you rendered!  ScalaJack takes great pains to try to ensure a read object matches the rendered original, but for Any this promise is not always possible to keep.

|Scala render() Type for Any|ScalaJack read() Type for Any|
|-------|-------|
|Byte |Long
|Short |Long
|Int |Long
|Long |Long
|BigInt |BigDecimal (if value too big for Long)
|Float |Double
|Double |Double
|BigDecimal |BigDecimal (if value too big for Double)

Remember that when processing Any, there is no "wrong"--any returned thing is an Any!  There's just expected and unexpected.