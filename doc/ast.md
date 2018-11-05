## Life with the AST

ScalaJack uses an AST (abstract syntax tree) internally to store what it knows about a class.  Everything inside ScalaJack is type-level programming on the AST using primitives on the AST.  In fact, ScalaJack doesn't even know *which* AST it's using!  Json4S is the default AST but others are possible.

### AST Types

Here are the data types supported by ScalaJack's AST type:

|Type|
|-------|
|AstArray
|AstBoolean
|AstDecimal
|AstDouble
|AstInt
|AstLong
|AstNull
|AstObject
|AstString

That's a nice set of basic types.  Arrays are basically Lists, and Objects are Maps.  The concept is that any "flavor" of ScalaJack would need to map these types to equivalent representations within the representation of the flavor.  For example for MongoDB's flavor we mapped these types into BSON equivalents so BsonValue is the core AST.

Again, ScalaJack knows nothing about JSON, BSON, or any other specific AST.  It operates entirely on these AST types.

### AST Operations

To complete the AST type there must be some basic operations.

#### foreachArrayElement(elements: ArrayElements, f: (Int, AST) => Unit): Unit
Pass over each element of an AstArray and execute some function, f, which is the array element zipped with its index.

_Example_
```scala
// where 'ops' is an instance of AstOps[AST,S] (passed implicitly to Deserializer/Serializer)
val actualElementsBuilder = List.newBuilder[AST]
ops.foreachArrayElement(arrayAST.asInstanceOf[ops.ArrayElements], { (index, element) =>
  actualElementsBuilder += element  // populate a List of elements
})
```

#### foreachObjectField(fields: ObjectFields, f: (String, AST) => Unit): Unit
Pass over each key/value pair in an object and execute function, f.

_Example_
```scala
val actualFieldsBuilder = List.newBuilder[(String, AST)]
ops.foreachObjectField(ops.unapplyObject(json).get.asInstanceOf[ops.ObjectFields], { (fieldName, fieldValue) =>
	actualFieldsBuilder += fieldName -> fieldValue
})
```

#### mapArrayElements[A](fields: ArrayElements, f: (Int, AST) => A): List[A]
Map over fields in an array.  Map function, f, returns type A so result of mapArrayElements() is List[A].

_Example_
```scala
val stuff = List(ops.applyInt(5), ops.applyLong(25L), ops.applyBoolean(true), ops.applyString("Fred"))
val myarr = ops.applyArray(stuff)
val intLike = ops.mapArrayElements(myarr.asInstanceOf[ops.ArrayElements], { (index, fieldValue) =>
  fieldValue match {
    case AstInt(i)  => i.intValue()
    case AstLong(n) => n
    case _          => 0
  }
})
```

#### mapObjectFields(fields: ObjectFields, f: (String, AST) => (String, AST)): ObjectFields
Map over object fields calling a function, f, which can alter the key/value aggregated to the resulting ObjectFields AST.  This is basically the same as map() below, except instead of a List[A] you are creating another ObjectFields with possibly different keys/values than the original.

_Example_
```scala
// Replace field name with canned id label if its a db key
val keyFieldName = dbKeys.head.name
ops.mapObjectFields(ast.asInstanceOf[ops.ObjectFields], { (fieldname, value) =>
  fieldname match {
    case s: String if s == keyFieldName => (idMemberName, value)
    case _                              => (fieldname, value)
  }
}).asInstanceOf[AST]
```

#### mergeObjectFields(fields1: ObjectFields, fields2: ObjectFields): ObjectFields
Smash two AST ObjectFields together into a new one.

_Example_
```scala
val stuff1 = List( ("a",ops.applyInt(5)), ("b", ops.applyLong(25L)) )
val one = ops.applyObject { appendField =>
  for ((fieldName, fieldValue) <- stuff1) {
    appendField(fieldName, fieldValue)
  }
}

val stuff2 = List( ("c",ops.applyBoolean(true)), ("d",ops.applyString("Fred")) )
val two = ops.applyObject { appendField =>
  for ((fieldName, fieldValue) <- stuff2) {
    appendField(fieldName, fieldValue)
  }
}

val both = ops.mergeObjectFields(one.asInstanceOf[ops.ObjectFields],two.asInstanceOf[ops.ObjectFields])
```

#### map[A](fields: ObjectFields, f: (String, AST) => A): List[A]
Map over object's fields, this time producing a List of some type, A.

_Example_
```scala
var count = 0
val foobar = ops.map { one, (fieldname, valueAst) =>
  count = count + 1 // anything here, using fieldname and/or valueAst, producing a result of type A
}
```

#### getObjectField(fields: ObjectFields, name: String): Option[AST]
Extract (if found) a field from ObjectFields of the given name.

_Example_
```scala
val found = ops.getObjectField(one, "b")
```

#### partitionObjectFields(fields: ObjectFields, fieldNames: List[String]): (ObjectFields,ObjectFields)
Partition an object's fields into two new ObjectFields based on a whether the field names are found in the given list.

_Example_
```scala
val (objHas, objDoesntHave) = ops.partitioinObjectFields(both, List("a","d"))
```

#### isObject(ast: AST): Boolean
Determine if a given AST value is an object.

#### isArray(ast: AST): Boolean
Determine if a given AST value is an array.
