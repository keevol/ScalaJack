# ScalaJack

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT) [ ![bintray](https://api.bintray.com/packages/blocke/releases/scalajack/images/download.svg) ](https://bintray.com/blocke/releases/scalajack/_latestVersion) [![Build Status](https://img.shields.io/travis/gzoller/ScalaJack.svg?branch=master)](https://travis-ci.org/gzoller/ScalaJack) [![Codacy branch grade](https://img.shields.io/codacy/grade/9437bb8b88464096b1a848ba0eed8b7d/master.svg?maxAge=2592000)](https://www.codacy.com/app/gzoller/ScalaJack?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=gzoller/ScalaJack&amp;utm_campaign=Badge_Grade) [![Coveralls branch](https://img.shields.io/coveralls/gzoller/ScalaJack/master.svg?maxAge=360)](https://coveralls.io/github/gzoller/ScalaJack)

ScalaJack is a very fast, seamless serialization engine for JSON, and other protocols, designed to require the minimum amount of help possible when serializing a class.

Advanced Features:
 - Handles tuples
 - 'Any' support
 - Handles default values for case class fields
 - Rich configuration of trait type hint/value
 - Supports value classes
 - Sealted trait-style enumerations
 - Extensible to other encodings (JSON, CSV, MongoDB, and DynamoDB are provided by ScalaJack, but you can roll your own too!)

## Use

ScalaJack is extremely simple to use.

Include it in your projects by adding the following to your build.sbt:

	libraryDependencies ++= Seq("co.blocke" %% "scalajack" % "6.0.0")

If you want to use the optional MongoDB serialization support include this as well:

	libraryDependencies ++= Seq("co.blocke" %% "scalajack_mongo" % "6.0.0")

DynamoDB helpers are available here:

	libraryDependencies ++= Seq("co.blocke" %% "scalajack_dynamo" % "6.0.0")

ScalaJack is hosted on Bintray/JCenter.  If you're using pre-v0.13.9 of SBT you may need to enable the bintray resolver in your build.sbt with

``` sbt
useJCenter := true
```

Now you're good to go!  Let's use ScalaJack in your project to serialize/de-serialize a case class object into JSON:

	import co.blocke.scalajack._

	val sj = ScalaJack()
	val js = sj.render( myCaseObj )  // serialization
	val myObj = sj.read[MyCaseClass](js) // deserialization

Couldn't be simpler!

## Features

* [Serialization Lifecycle and Granular Control](doc/lifecycle.md)
* [Case Classes and Traits](doc/classesAndTraits.md)
* [Non-Case Classes and Java Class Support](doc/noncase.md)
* [Any Support](doc/any.md)
* [Value Class Support](doc/valueClass.md)
* [Parameterized Classes](doc/parameterized.md)
* [Trait Type Hint Customization](doc/typeHint.md)
* [Life with the AST](doc/ast.md)
* [Custom Type Adapters (custom read/render)](doc/custom.md)
* [Try and Capture](doc/tryAndCapture.md)
* [ParseOrElse](doc/parseOrElse.md)
* [Null and None treatment](doc/nullAndNone.md)
* [Non-Canonical JSON](doc/noncanonical.md)
* [Externalized Type Hints](doc/externalTypes.md)
* [Re-name Case Class Fields in JSON or Mongo](doc/mapname.md)

Non-JSON Formats:
* [MongoDB](doc/mongo.md)
* [CSV](doc/csv.md)
* [DynamoDB](doc/dynamo.md)

## Benchmarks

|Benchmark         |Score      |Error        |Units
|------------------|----------:|------------:|-----|
|Hand-written      |28683.250  |± 3505.351   |ops/s
|**ScalaJack 5.0** |20632.580  |±  306.105   |ops/s
|Spray             |10314.990  |±  120.898   |ops/s
|LiftJson          |9313.326   |±  212.206   |ops/s
|ScalaJack 4.8.3   |6525.699   |±  36.103    |ops/s
|Json4s            |5840.046   |±  201.42    |ops/s

## Series 6

Series 5 introduced a whole new engine for ScalaJack.  For series 6 we went through and streamlined the the whole thing!  JSON is no longer assumed in the core, allowing for an easier extension to other protocols, which we tested especially for MongoDB.  Internally the code is tighter and cleaner, which always makes us feel happy.  Perhaps the biggest change is the use of an AST (abstract syntax tree) to hold knowledge about a reflected class.  Although Json4S is the default, you can even specify a different AST if you want.

Another improvement is in error reporting.  Backward compatibility is preserved with read()/write(), but if you'd like more details about read failures you can use the readSavely() function, which returns Either[DeserializationFailure,T], so you can see what actually broke.

We hope you'll enjoy using the latest ScalaJack!

*Blöcke*
