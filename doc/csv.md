## CSV

ScalaJack offers CSV serialization support, but it must be said clearly that there are limiting structural constraints to the features supported:

* Class parameters must be flat (scalars only)!  CSV has no ability to represent structured or nestable content like collections.
* Traits are not supported (where would you put the type hint in CSV format?)
* You must enclose a field with double-qutoes if it contains double quotes, commas, or line breaks.
* Double quotes within a string/field must be escaped using double-double quotes, "" (not the more common \")
```scala
val sj = ScalaJack(CSVFlavor())
val inst = StringHolder("Hey, you", "This \"life\"", "And now, \"Mike\" will sing.")
val csv = sj.render(inst)
// renders: "Hey, you","This ""life""","And now, ""Mike"" will sing."
```

### Handling None and Null
If an object has an Option field with value None, then ScalaJack will renders an empty field.  Note this is different than an empty String (quotes must be included in the CSV for empty string or a blank field will just be a null/None).  Consider the following CSV creation:

```scala
case class Maybe(one:String, two:Option[String], three:Boolean)

val sj = ScalaJack(CSVFlavor())

val inst = Maybe("yes", Some("This,test"), true)
val csv = sj.render(inst)
val i1 = sj.read[Maybe](csv)
// renders: yes,"This,test",true

val inst2 = Maybe("yes", Some(""), true)
val csv2 = sj.render(inst2)
val i2 = sj.read[Maybe](csv2)
// renders: yes,"",true

val inst3 = Maybe("yes", None, true)
val csv3 = sj.render(inst3)
val i3 = sj.read[Maybe](csv3)
// renders: yes,,true

val inst4 = Maybe(null, null, true)
val csv4 = sj.render(inst4)
val i4 = sj.read[Maybe](csv4)
// renders: ,,true
// WARNING: Reconstitutes Maybe(null,None,true)

val inst5 = Maybe("", null, true)
val csv5 = sj.render(inst5)
val i5 = sj.read[Maybe](csv5)
// renders: "",,true
```

**Warning:** Did you catch inst4?  Null and None are both rendered as empty CVS fields for Option values.  This means if you try to read an empty CSV field into an Option Scala field, it always reads in as None, even if the original was null.  There's no way to represent a null value for an Optional value in CSV.  (Not a problem, since we shouldn't be using nulls anyway, right?   Right?)

For other values empty CSV fields becomes null.

|Field Value |Option |String |
|------|-------|-----|
|""|Some("")  |""
|(empty)|None |null  

