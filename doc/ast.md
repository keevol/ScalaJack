## Life with the AST

ScalaJack uses an AST (abstract syntax tree) internally to store what it knows about a class.  Everything inside ScalaJack is type-level programming on the AST using primitives on the AST.  In fact, ScalaJack doesn't even know *which* AST it's using!  Json4S is the default AST but others are possible.

### AST Types

The AST types are held by an internal representation (IR).  Here are the data types supported by ScalaJack's IR:

|Type|
|-------|
|IRArray
|IRBoolean
|IRCustom
|IRDecimal
|IRDouble
|IRInt
|IRLong
|IRMap
|IRNull
|IRObject
|IRString

That's a nice set of basic types.  Arrays are basically Lists.  Objects and maps differ in that Objects require the key to be a String, while a Map's key can be anything.  The concept is that any "flavor" of ScalaJack would need to map these types to equivalent representations within the representation of the flavor.  For example for MongoDB's flavor we mapped these types into BSON equivalents so BsonValue is the core AST.

ScalaJack internals know nothing about JSON, BSON, or any other specific AST.  It operates entirely on these AST types.

### Implementing Your Own AST

If you want to implement your own AST for ScalaJack (i.e. create a new flavor), you'll need to implement a few things.  Fortunately there are good examples for JSON, CSV, MongoDB/BSON, and Dynamo here in the ScalaJack github repo.

1. Implement the WireTransceiver trait (serialize/deserialize from IR).  This connects the IR to the output "WIRE" format (e.g. JSON).
2. (optional) Implement the OpsBase trait, which maps all the basic IR types to the specific, concrete types in your desired AST.  This is optional if you want to re-use an existing one, Json4S for example.
3. Implement the Ops trait by mixing an OpsBase with a WireTransceiver.
4. Write an implementation of JackFlavor

You're good to go!  If you have custom types or you want existing types to have special handling, you can write TypeAdapters (see MongoDB for examples).  Wire any custom TypeAdapters by overriding bakeContext() in JackFlavor and putting your custom adapters at the front of the TypeAdapterFactories list.

