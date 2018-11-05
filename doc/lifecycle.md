## Serialization Lifecycle and Granular Control

ScalaJack utilizes an AST (abstract syntax tree) internally to hold information learned by reflecting on your serialized class.  This allows more control over the serialization lifecycle, as shown in the graphic below.

![Lifecycle](https://github.com/gzoller/ScalaJack/doc/lifecycle.jpg "Serialization Lifecycle")

Let's go through the different controls you have.

### Simple read/render
This is the most basic and easy way to use ScalaJack.
```scala
val sj = ScalaJack()  // default to JSON serialization (JsonFlavor)

val myCaseClass = Maybe("yes", Some("This,test"), true)

val json = sj.render(myCaseClass)

val inst = sj.read[Maybe](json)
```
What's going on here?  First we create an instance of ScalaJack, which defaults to JSON serialization.  (You can see other serializations elsewhere in this documentation, but they work fundamentally the same way.)  Then we create a case class instance.  So far so good.  Then we render the instance to JSON (String).  This is the top render() line in the picture--going from an instance to a rendered thing, JSON in this case.  The AST internal stuff is happening, but in the simple mode, you don't care.

Finally we can read the JSON back into a Scala case class.  You do need to tell ScalaJack what the case class (or trait) is supposed to be though--it can't read your mind.  Here we tell it the JSON is supposed to be a serialized Maybe class.

##### Read Error Details
If there's a problem reading your JSON back into the requested class type, ScalaJack throws a DeserializationException.  For example if you provided a class in read that didn't match the given JSON.  If you'd rather avoid exception handling try this:

```scala
val inst = sj.readSafely[Maybe](json)  // returns Either[DeserializationFailure, T]
inst match {
   case Right(result) => result
   case Left(oops) => // handle error
   }
```

The key pieces in DeserializationFailure you'll care about are:

```scala
  def errors(path: Path): immutable.Seq[(Path, DeserializationError)]
  def isUnsupported(path: Path): Boolean
  def isUnexpected(path: Path): Boolean
```

This will given you details.  You can have more than one error during read, so errors is a Seq.  Path tells you where in the scheme of things something went wrong, and DeserializationError contains a message field telling you what happened.

### Emit/Parse
Maybe you don't care about the Scala classes?  Perhaps you'd like to go back 'n forth between the rendered output (e.g. JSON) and the AST, allowing you to view or manipulate the AST.  That's where these functions come in:

```scala
val ast = sj.parseToAST(json)
val json2 = sj.emitFromAST(ast)
```
There's no Scala class involvement at all.  This might be just what you want in some cases, for example if you have code that acts as a filter, augmenter, or router for serialized content without the need to pull all the way back to Scala (or if you don't even have the matching Scala jar files for that content!).

The default AST for JsonFlavor is Json4s, so you  can use all the wonderful tools and DSL provided by that library if you want to modify the AST.

### Materialize/Dematerialize
This is the "front-end" to Emit/Parse.

```scala
val ast = sj.dematerialize()
val inst2 = sj.materialize[Maybe](ast)
```

This time we don't bother with any kind of serialized output.  We're just using the AST as a way to manipulate Scala objects.  This can often be easer than managing Scala reflection yourself... let ScalaJack do the work!  For a great example of how this is useful, check out ScalaJack.scala in the view() and spliceInto() fucntions, where we actually use this ourselves.


