## View / SpliceInto

ScalaJack alrady provides a number of features that allow you to support rich serialization needs.  For this feature we've practiced what we preach by developing a feature utilizing the AST manipulation facilities provided by ScalaJack you can use for your custom capabiities.

The use case for view/spliceInto is where you have a msster class, which may contain some system-private information.  We also need a 'lite' version of the master class for transport to a UI, but the lite version must not contain any of the private information.  We want the lite version to be a "view" (projection) of the master class.  If the UI modifies the lite version, we want to splice its changes back into the master.

First the classes:
```scala
case class Master(
    name:          String,
    id:            Long,
    underwearSize: Char,
    favorites:     List[String]
    )

case class Subset(
    name:          String,
    id:            Long,
    underwearSize: Char,
    favorites:     List[String]
    )
```

Here we have a Subset class that doesn't include the person's underwearSize.  Now let's see how to project that view:

```scala
val master = Master("Fred",123L, 'M', List("music","football"))
val subset = sj.view[Subset](master)
```

We now have a Subset object that's "safe" to transport.

Now let's assume that something has modified Subset and we want to recombine it back with Master.  We can do it this way:

```scala
val newMaster = sj.spliceInto(modifiedSubset, master)
```

Easy, right?

Checkout ScalaJack.scala to see the implementation of view and spliceInto. They're great examples of how to use AST manipulation to build features like this of your own.