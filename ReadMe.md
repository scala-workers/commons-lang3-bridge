# Commons Lang3 Scala Bridge

[![Maven Central](https://img.shields.io/maven-central/v/net.scalax/commons-lang3-bridge_3.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22net.scalax%22%20AND%20a:%22commons-lang3-bridge_3%22)

A scala bridge library from apache commons lang3 to scala.

## Usage

Add dependence into sbt like:

```scala
libraryDependencies += "net.scalax" %% "commons-lang3-bridge" % "0.1.0"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.12.0"
```

or maven:
```xml
<dependency>
    <groupId>net.scalax</groupId>
    <artifactId>commons-lang3-bridge_3</artifactId>
    <version>0.1.0</version>
</dependency>
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-lang3</artifactId>
  <version>3.12.0</version>
</dependency>
```

*Commons lang3 bridge support scala 2.11, 2.12, 2.13, 3.1, and 3.2, choice edition by prefix.*

Add import in your source:

```scala
import commons.lang3.bridge.StringUtils.ops._
```

Now, you can call commons lang string utils functions as String or `Option[String]`'s method like:

```scala
  test("test string substring after last separator char") {
    assert(nullString.ops.substringAfterLast(0).isEmpty)
    assert(noneString.ops.substringAfterLast('X').isEmpty)
    assert("".ops.substringAfterLast(0).contains(""))
    assert(Some("").ops.substringAfterLast('a').contains(""))

    assert("foo".ops.substringAfterLast(0).contains(""))
    assert("foo".ops.substringAfterLast('b').contains(""))
    assert(Some("foot").ops.substringAfterLast('o').contains("t"))
    assert("abc".ops.substringAfterLast('a').contains("bc"))
    assert("abcba".ops.substringAfterLast('b').contains("a"))
    assert("abc".ops.substringAfterLast('c').contains(""))
    assert("".ops.substringAfterLast('d').contains(""))
  }
```

## What's it

[Apache Commons Lang](https://commons.apache.org/proper/commons-lang/) provides a host of helper utilities for the 
java.lang API, notably String manipulation methods, basic numerical methods, object reflection, concurrency, creation 
and serialization and System properties. Additionally it contains basic enhancements to java.util.Date and a series 
of utilities dedicated to help with building methods, such as hashCode, toString and equals.

Commons Lang3 Bridge is a Scala bridge library. It supported use apache commons lang as scala style.

## StringUtils

With Apache Commons StringUtils, we call functions as:

```java
    public void testDefaultIfEmpty_StringString() {
        assertEquals("NULL", StringUtils.defaultIfEmpty(null, "NULL"));
        assertEquals("NULL", StringUtils.defaultIfEmpty("", "NULL"));
        assertEquals("abc", StringUtils.defaultIfEmpty("abc", "NULL"));
        assertNull(StringUtils.getIfEmpty("", null));
        // Tests compatibility for the API return type
        final String s = StringUtils.defaultIfEmpty("abc", "NULL");
        assertEquals("abc", s);
    }
```

It's strong and power, null safe with all side conditions.

### Extensions

And now, in scala, we add a extern field named `ops` for `String` and `Option[String]` type. 
Call the functions like OO style methods:

```scala
import org.scalatest.funsuite.AnyFunSuite
// import bridge extensions
import commons.lang3.bridge.StringUtils.ops._
// ...
class StringUtilsSpec extends AnyFunSuite {
  // ..
  test("test string default if empty with string") {
    assert(nullString.ops.defaultIfEmpty("NULL") == "NULL")
    assert(Some("").ops.defaultIfEmpty("NULL") == "NULL")
    assert("abc".ops.defaultIfEmpty(Some("NULL")) == "abc")
    assert("".ops.defaultIfEmpty(null) == null)
    // Tests compatibility for the API return type
    val s = "abc".ops.defaultIfEmpty("NULL")
    assert(s == "abc")
  }
}
```

### Null safe and TypeMapping

All StringUtils functions in Apache Commons Lang3 is null safe. And the bridge bundle it as null and None safe.

Specifically:
 - All functions return String may null that bridged to returns `Option[String]` 
 - All args accepted a String may null that bridged to accepted `Option[String]` or String. 
You can choose any way as you wish. The bridged methods auto handle them!
 - When you see a source in document as `none`, It could be a `String` variable as null, or a 
`Option[String]` variable as None. In the bridge core, the powerful TypeMapping mechanism 
known howto convert it. 
 - Some functions have varargs typed `String[]`. We bridged them as scala varargs `Seq[(String|Option[String])]*`.
The bridge choice String or `Option[String]` what ever your pass in. But they must all `String` 
or all `Option[String]` same. Don't mix difference type in one varargs Seq.
 - Some functions have two String parameters. By the bridge you can call them pass etc.
`str.ops.method(String, Option[String])` or `str.ops.method(Option[String], String)` or 
what ever combinators. Because the parameters have independents type definitions. 
 - Some functions are accepted parameters as `CharSequnces`. By bridged you can pass 
`CharSequnces` or `Option[CharSequence]` as your wish.

### Adt Type Handle

Many methods in bridge (They have written in `StringCommons.scala` source) with type parameters like:

```scala
  def removeEnd[R: Adt.Options2[*, String, Option[String]]](rmv: R): Option[String] = {
      val rmvStr = mapToStrOpt.input(rmv).orNull
      Option(Strings.removeEnd(strOrNull, rmvStr))
  }
```

The type means a union type `String` or `Option[String]`. 

### Limit

Not all functions in StringUtils class were been bridged. 

We skip something like:
 - All deprecated functions
 - Something like `isNoneBlank`、`isAnyEmpty` and so on. They aren't oo style code 
have subject, predicate and objects.
 - All join method. Not only isn't OO style but also scala has powerful string function `mkString` . 
 - Bridge support Union Type definition `(String | Option[String])`，but almost char args 
just bundled as `Char`, without `Option[Char]`. Because `Char` is a primitive type.
 - Some functions received char args as int type. The bridged bundled it as Char or Int.  
You need recognize it in scala.
 - In apache commons, when you pass null literal into functions, them returns result safely. 
But the bridged methods need now what's type of them. So pass a null `String` or None 
`Option[String]` this safe, `null` literal without type is *exception*.


