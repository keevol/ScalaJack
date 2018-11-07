## Null and None Handling

Representing nulls and None is problematic in JSON because while the spec provides for null it offers nothing to represent None.  That forces us to sometimes make inconsistent, contextualized assumptions about how best to represent these concepts.

|Usage  |Example|JSON Representation
|-------|-------|-------------
|Class member|```MyClass(None,3)```|{"age":3}  *(ignored None field)*
|List|```List(Some(1),None,Some(3))```|[1,null,3] *(None converted to null)*
|Map value|```Map("a"->Some(1),"b"->null)```|{"a":1} *(None converted to null)*
|Map key|```Map(Some(1)->"a",None->"b",Some(2)->"c",None->"d")```|{"1":"a","":"d","2":"c"} *(empty string)*
|Tuple member|```(5,None,"hey")```|[5,null,"hey"] *(None converted to null)*

A Scala Map can indeed have None as a valid key so in JSON we can't just drop that key/value pair from the Map.  Canonical JSON requires Map keys to be strings, so "" is the logical (only) choice.   __NOTE:__ If you have a Map with >1 keys == None, only one of these Map entries will survive (keys must be unique!).  See the example in the table above: None->"b" and None->"d".  One of the entries disappears--no guarantee which, so be careful!

Tuples represent None as null as an awful compromise.  We can't drop the value from the tuple because it has a fixed size that must be requested in the JSON.  JSON can't handle an empty field, i.e. "[1,,3]", so the only reasonable option left is null.  

**Special Note**
If this isn't already confusing, it gets slightly more terrible.  Parsing JSON null or "" back into Scala is complicated by the type of field.  

In a Map key, if the key's type is String, a "" parses into an empty String.  If the Map key's type is Option[], then "" parses to None.  Thankfully, null seems to be disallowed as a Map key in Scala!

In a tuple, if the member's type is Option[], a null parses as None, otherwise it parses as null.

Just be careful!  You may not get exactly the object you expect.  Worse, if you render an object to JSON, then re-read that object it may differ from the original in these special cases.

Clear as mud?