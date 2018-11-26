## ScalaJack Configuration and Usage

### ScalaJack Instantiation
All ScalaJack usage starts with creating a ScalaJack instance.

```scala
val sj = ScalaJack()
```

This instance is used for all serialization activities.

### Flavors

ScalaJack supports several "flavors": JSON, CSV, MongoDB, and DynamoDB out of the box.  Others may create new flavors by extending the pattern.

Here's how to get the right flavor of ScalaJack for your needs:

|Flavor |ScalaJack instantiation
|-----|--------
|JSON | val sj = ScalaJack()   // JSON is default if no flavor given
|CSV  | val sj = ScalaJack(CSVFlavor())
|MongoDB | val sj = ScalaJack(MongoFlavor())
|DynamoDB | val sj = ScalaJack(DynamoFlavor())

Note that MongoDB and DynamoDB require a separate library, and are not included in the core ScalaJack package.

### ScalaJack Configuration Methods
ScalaJack can be configured using "chain" methods, i.e. you chain them together, for example:

```scala
val sj = ScalaJack()
  .parseOrElse((typeOf[Person],typeOf[DefaultPerson]))
  .withDefaultHint("kind")
```
Most of these configurations are JSON-only, as they don't make sense for the other formats.  An exception will be thrown if a configuration method is used that isn't supported for a particular ScalaJack flavor.

#### parseOrElse(poe: (Type, Type)*)
Configures a default object to be returned if any object of the given type can't be parsed successfully.  This is a mapping, so multiple specifications are possible.

```scala
val sj = ScalaJack()
  .parseOrElse( (typeOf[Address], typeof[UnknownAddress), (typeOf[Command], typeOf[DoNothingCmd]) )
```

#### withAdapters(ta: TypeAdapterFactory*)
Register a list of custom TypeAdapters with ScalaJack.  This is to allow you to offer custom serialization handling of your own types.

[See an example.](custom.md)

#### withDefaultHint(hint: String)
This simply changes (globally) all the default type hint key strings.  This can be mixed with withHints().  Any type not specified in withHints will pick up your re-set default hint.

#### withHints(h: (Type, String)*)
Specify per-type hints to override the default type hint of "_hint".
__Tip:__ To get the type of a class, for example to use with this function, you can use:
```scala
typeOf[MyClass]
```

[See an example.](typeHint.md)

#### withHintModifiers(hm: (Type, HintModifier)*)
Where withHints modifies the key String for a type hint, withHintModifiers modifies the hint's value.  Particiularly handy for 3rd party JSON where you don't own/control the values and you want a function to line it up with your internal representation.

[See an example.](typeHint.md)

#### withTypeModifier(tm: HintModifier)
This is used to modify the string identifying the externalized type hint (type member) of a class.  This is global, I'm afraid, so anything you specifiy here will apply to all type members serialized with this instance of ScalaJack.

[See an example.](externalTypes.md)

### withSecondLookParsing()
Some 3rd party JSON can be messy!  They may make booleans and numbers into strings.  withSecondLookParsing() allows ScalaJack to be a little less strict and try to accept "true" as true, for example.

```scala
val js = """["true","false"]
val sj1 = ScalaJack()
val sj2 = ScalaJack().withSecondLookParsing()

sj1.read[List[Boolean]](js) // explodes with error: expected Boolean not String

sj2.read[List[Boolean]](js) // reads List(true,false)
```
