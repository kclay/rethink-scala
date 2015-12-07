[![Build Status](https://travis-ci.org/kclay/rethink-scala.png)](https://travis-ci.org/kclay/rethink-scala)

Scala Rethinkdb Driver
=========

_Work in progress for RethinkDB 2.0 release._

FEATURES

 - Full Type Safety (still a work in progress, will use macros to support case class type safety, right now all queries should be typed checked against the rules of RQL, )
 - Mapping to and from case classes , this allows you to fetch data to an case class via .as[CaseClass] or insert data from case classes (will be translated to a Map[String,_]
 - Lazy evaluation, all expressions are evaluated in a lazy fashion, meaning that the ast will be build up but the inner `args` and `optargs` wont be resolved until .run/.as or .ast is called for performance.
 - Importing com.rethinkscala.Implicits._ will give you a more normal way to construct your rql so you can write normal scala code without worrying about casting in a `Typed` or via `Expr`
 - Uses Jackson for json mapping via [jackson-module-scala](https://github.com/FasterXML/jackson-module-scala) 

# Guide
* [Getting Started](https://github.com/kclay/rethink-scala/wiki/Getting-Started)
* [Type Safety](https://github.com/kclay/rethink-scala/wiki/Type-Safety)


TODO

  - Complete Test Suite
  - Fix compile warns
  - Allow type safety for `Predicate` classes via macros


SBT Users
```scala
val main = Project(....).settings(resolvers ++= Seq("RethinkScala Repository" at "http://kclay.github.io/releases"))

val rethinkscala = "com.rethinkscala" %% "core" % "0.5.0",
val rethinkscala = "com.rethinkscala" %% "core" % "0.5.1-SNAPSHOT"
```
To get started
```scala
import com.rethinkscala.Blocking._ // for blocking api
implicit val blockingConnection = Blocking(Version3)
import com.rethinkscala.Async._ // for async api
implicit val asyncConnection = Async(Version3)
```

Examples

```scala

scala> r.table("marvel").map(hero=> hero \ "combatPower" + hero \ "combatPower" * 2)
res2: com.rethinkscala.ast.RMap = RMap(Table(marvel,None,None),Predicate1(<function1>))


scala> import com.rethinkscala._
import com.rethinkscala._

scala> val version =new Version3("172.16.2.45")
version: com.rethinkscala.net.Version3 = Version3(172.16.2.45,28015,None,5)

scala> implicit val connection = new Connection(version)
connection: com.rethinkscala.Connection = Connection(Version2(172.16.2.45,28015,None,5))

scala> val info =DB("foo").table("bar").info
info: com.rethinkscala.ast.Info = Info(Table(bar,Some(false),Some(DB(foo))))

//case class DBResult(name: String, @JsonProperty("type") kind: String) extends Document
//case class TableInfoResult(name: String, @JsonProperty("type") kind: String, db: DBResult) extends Document

scala> val result = info.as[TableInfoResult]
result: Either[com.rethinkscala.net.RethinkError,com.rethinkscala.net.TableInfoResult] = Right(TableInfoResult(bar,TABLE,DBResult(test,DB)))

// selecting data
scala> r.db("test").table("foos").create.run
res1: Either[com.rethinkscala.net.RethinkError,Boolean] = Right(true)

scala> val table = r.db("test").table("foos")
table: com.rethinkscala.ast.Table = Table(foos,Some(false),Some(DB(test)))

scala> val records = for(i <-1 to  5) yield SelectFoo(i)
records: scala.collection.immutable.IndexedSeq[SelectFoo] = Vector(SelectFoo(1), SelectFoo(2), SelectFoo(3), SelectFoo(4), SelectFoo(5))

scala> table.insert(records).run
res2: Either[com.rethinkscala.net.RethinkError,com.rethinkscala.net.InsertResult] = Right(InsertResult(5,0,0,0,None,null,0,0))

scala> val results = table.between(2,4).order("id").as[SelectFoo]
results: Either[com.rethinkscala.net.RethinkError,Seq[SelectFoo]] = Right(Cursor(SelectFoo(2), SelectFoo(3), SelectFoo(4)))

scala> val results = table.filter(f=> f \ "id"> 2).as[SelectFoo]
results: Either[com.rethinkscala.net.RethinkError,Seq[SelectFoo]] = Right(Cursor(SelectFoo(3), SelectFoo(5), SelectFoo(4)))

scala> val results = table.filter(f=> f \ "id"> 2).order("id".desc).as[SelectFoo]
results: Either[com.rethinkscala.net.RethinkError,Seq[SelectFoo]] = Right(Cursor(SelectFoo(5), SelectFoo(4), SelectFoo(3)))


```

More found at [here](https://github.com/kclay/rethink-scala/blob/master/core/src/test/scala/example/blocking/Tutorial.scala)
Be sure to check out the [wiki](https://github.com/kclay/rethink-scala/wiki)



Installation
--------------
Note needs protobuf 2.5.0 installed

```sh

Checkout Main repo
sudo apt-get install protobuf-compiler
git clone git@github.com:kclay/rethink-scala.git
cd rethink-scala
sbt compile

```

Version
-
###0.4.9 - 12/07/15
- Update to Scala 2.11.7, SBT 0.13.8, and target JVM 1.8 (thanks @crockpotveggies)
- Added  scalaz-stream support
- Refactored connections to have `Backends`, this will allow switching connection/stream implementations
- Updated to support rethink API 2.0


###0.4.6 - 05/12/15
- Polymorphism support #23
- No need to extend `Document` anymore #19
- Breaking Change - table.getAll(indes:String,attr:Any*) is now table.getAll(attr:Any*).withIndex(index:String)
- Breaking Change - Updated table.indexCreate definition
- Support for Continuing a stream
- Added functional blocking delegate which uses `Try`
- Updated Netty.io to 4.0.27.Final
- Fixed issues with connection pooling where results wouldn't resolve and timeout.
- Breaking Change - casting an Var to map is now done with `mapOf[T]`
- Made connection setup sequence fully async
- Blocking._ and Async._ now has Version3 visible
- Added more [examples/tutorial](https://github.com/kclay/rethink-scala/blob/master/core/src/test/scala/example/blocking/Tutorial.scala) (thanks @Shegnc )
###0.4.6 - 01/29/15
- Geo Support
- Uuid Support
- Fixed restoring connection on failed quries (thanks @mick-h)
- Fixed Option support (thanks @mick-h)
- Fixed issue were null results were not handle properly (thanks @mick-h)
- Fixed nested document insert (thanks @maohde)
- Fixed exception being thrown when query returns back empty results (thanks @mick-h)

###0.4.5 - 10/12/14
- Fixing Nested documents for json
- Fixed connection pooling
- Minor bug fixes

###0.4.4 - 8/26/214
- JSON Support
- Start of HTTP Support
- Updated to support 1.13.x

###0.4.3 - 9/24/13
 - Added zip
 - bug fixes

###0.4.2 - 08/12/13
 - Fixed update and filter quries

###0.4 - 08/09/13
 - Fixed race conditions in connection pool
 - Switched to using protobuf-java 2.4.1 due to compatibility issues with akka-cluster 2.2.0
 - minor bug fixes

###0.3 - 07/28/13
 - A `com.rethinkscala.Schema` object can be created , this provides some helper methods to provide type-saftey for tables
 - The map representation of the query is returned as well if cased to `com.rethinkscala.net.Document` and is accessable by using a a `com.rethinkscala.net.DocPath`
 - Active record type update and saves are not available, importing you schema into scope provides your case classes with an `save` and `update` method, `update` does a full replace for now
 - Case classes can now have their `id` automatically generated by providing a `id:Option[String] = None`
 - Documents now have a beforeInsert and afterInsert lifecycle, afterInsert/afterInsert(generatedKey:String)
 - Driver now supports 1.7.x which allows you to fetch the `new_vals` for single entry update/insert via ChangeResult.resultValue[T]
 - minor bug fixes

###0.2 - 07/04/13

Streams are now supported, this means that when using methods like `.getAll` and `between` the results will be wrapped in a `com.rethinkscala.net.Cursor[R]` which will act like an `Seq[R]`

###0.1 - 
Initial release
