## Non-Case and Java Classes

In the last section we saw how easily ScalaJack handles case classes and Scala traits.  It can handle non-case classes and Java classes too.  The Scala case class is a fantastic thing, and does a lot behind the scenes on your behalf, but if you follow certain conventions, ScalaJack can work with a non-case and Java class.

The big challenge for ScalaJack is to know what the members, aka fields, of a class are.  In a case class this is easy: they're given in the constructor.  In a non-case class they *may* be in the constructor--or not.  We can't rely on this, so...

#### Scala Non-Case Class val constructor (most preferred method)
If you have a non-case Scala class, and you specify 'val' for each of the constructor arguments, ScalaJack will treat it just as a case class.  But... **all** the arguments but be specified with val!

```scala
// Works!
class Employer( val name:String, val est:java.time.LocalDate )

// Doesn't work... not all arguments specified with val
class Employer( name:String, val est:java.time.LocalDate )
```

Note this method of detecting class fields trumps all the others, so even if you use any of the other mechanisms, they'll be ignored in favor of val constructor fields if at least one is provided.

#### Scala Non-Case Class with getters and setters (ok method)
Another way ScalaJack can detect your class fields using standard Scala getter/setter format with a private var field:

```scala
class Employer() {
  private var _name:String = ""
  def name: String = _name
  def name_=(n:String) = _name = n

  private var _est:LocalDate = LocalDate.now()
  def est: LocalDate = _est
  def est_=(e:LocalDate) = _est = e
}
```

If you have any fields with either a getter or setter that you do *not* want serialized, put an @Ignore annotation on either the getter or setter and ScalaJack will ignore that field.

#### Scala Non-Case Class with public var (least preferred method)
A final way to specify your Scala class fields is to define them as public var fields, which is bad for obvious reasons of no data hiding.

```scala
class Employer() {
  var name:String = ""
  var est:LocalDate = LocalDate.now()
  var profitMargin:Double = 21.0
}
```

ScalaJack finds these fields and treats them as class fields.  You can mix this method with the getter/setter method.

ScalaJack's auto-detection of all var fields means that *all* public var fields are treated as class members!  This may not be what we want, so ScalaJack provides an @Ignore annotation to let you ignore var fields you don't want detected and treated as class members:

```scala
class Employer() {
  var name:String = ""
  var est:LocalDate = LocalDate.now()
  @Ignore var profitMargin:Double = 21.0
}
```

As you know, exposing all this stuff isn't really good OO or functional programming, so we strongly recommend against using the public vars method of field detection.

#### Java Class Support
ScalaJack's support for Java classes is more limited due to Java's looser handling of types and constructors.  In order for Scala to detect your class fields your Java class must fit standard JavaBean notation for getters and setters (BeanInfo is used inside ScalaJack to detect your getters and setters):

```java
public class PlayerJava {
	private String name;
	private int age;

	public String getName() { return name; }
	public void setName(String n) { name = n; }
	public int getAge() { return age; }
	public void setAge(int a) { age = a; }
}
```
You can also use the @Ignore annotation on either a getter or setter if you don't want a JavaBean field serialized.