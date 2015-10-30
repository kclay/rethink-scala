package com.rethinkscala

import com.rethinkscala.ast._

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 8/17/13
 * Time: 9:59 AM
 *
 */
trait Options {
  def toMap: Map[String, Option[_]]
}


case class TableOptions(
                         durability: Option[Durability.Value] = None,
                         primaryKey: Option[String] = None,
                         shards: Option[Int] = None,

                         dataCenter: Option[String] = None,
                         replicas: Either[Int, Map[String, Any]] = Left(1),
                         primaryReplicaTag: Option[String] = None

                       ) extends Options {
  def toMap = Map("primary_key" -> primaryKey, "shards" -> shards,
    "datacenter" -> dataCenter,
    "replicas" -> Some(replicas.fold[Any](identity, identity)),
    "durability" -> durability,
    "primary_replica_tag" -> primaryReplicaTag)
}

case class QueryOptions(
                         useOutdated: Option[Boolean] = None,
                         noreply: Option[Boolean] = None
                       ) extends Options {
  def toMap = ???
}


case class InsertOptions(
                          durability: Option[Durability.Value] = None,
                          returnValues: Option[Boolean] = None,
                          upsert: Option[Boolean] = None,
                          returnChanges: Option[Boolean] = None,
                          conflict: Option[Conflict.Kind] = None
                        ) extends Options {
  def toMap = Map("upsert" -> upsert,
    "conflict" -> conflict,
    "durability" -> durability, "return_changes" -> returnChanges)
}


case class UpdateOptions(
                          nonAtomic: Option[Boolean] = None,
                          durability: Option[Durability.Value] = None,

                          returnChanges: Option[Boolean] = None
                        ) extends Options {
  def toMap = Map("non_atomic" -> nonAtomic, "durability" -> durability, "return_changes" -> returnChanges)
}


case class BetweenOptions(index: Option[String] = None,
                          leftBound: Option[Bound.Value] = None,
                          rightBound: Option[Bound.Value] = None)
  extends Options {
  def toMap = Map("index" -> index, "left_bound" -> leftBound, "right_bound" -> rightBound)
}

case class BoundOptions(
                         leftBound: Option[Bound.Value] = None,
                         rightBound: Option[Bound.Value] = None
                       ) extends Options {
  def toMap = Map("left_bound" -> leftBound, "right_bound" -> rightBound)
}

object Bound extends Enumeration {
  type Kind = Value
  val Open = Value("open")
  val Closed = Value("closed")
}

//case class DuringOptions

object Durability extends Enumeration {
  type Kind = Value
  val Hard = Value("hard")
  val Soft = Value("soft")
}

object Conflict extends Enumeration {
  type Kind = Value
  val Error = Value("error")
  val Replace = Value("replace")
  val Update = Value("update")
}

object ReadMode extends Enumeration {
  type Kind = Value
  val Majority = Value("majority")
  val Single = Value("single")
  val Outdated = Value("outdated")
}


trait RequestOptionsOps {

  import com.rethinkscala.ast.HttpMethod._

  type OptionType

  def lift(f: RequestOptions => RequestOptions): OptionType

  def asPost = lift(_.copy(method = Some(POST)))

  def asGet = lift(_.copy(method = Some(GET)))

  def asHead = lift(_.copy(method = Some(HEAD)))

  def asPatch = lift(_.copy(method = Some(PATCH)))

  def asPut = lift(_.copy(method = Some(PUT)))


  def withHeader(headers: HttpHeader*) = :=(headers)

  def withHeaders(headers: HttpHeaders) = :=(headers)

  def :=(headers: HttpHeaders) = lift(_.copy(headers = headers))

  def :=(headers: Seq[HttpHeader]) = lift(_.copy(headers = HttpHeaders(headers)))

  def ++(headers: HttpHeaders) = lift(o => o.copy(headers = o.headers ++ headers))

  def ++(headers: Seq[HttpHeader]) = lift(o => o.copy(headers = o.headers ++ headers))

  def addHeaders(headers: HttpHeaders) = ++(headers)

  def addHeaders(headers: HttpHeader*) = ++(headers)

  def withBasic(user: String, pass: String) = lift(_.copy(auth = Some(BasicAuth(user, pass))))

  def withDigest(user: String, pass: String) = lift(_.copy(auth = Some(DigestAuth(user, pass))))

  def withData(data: HttpBody[_]) = lift(_.copy(data = Some(data)))
}

case class RequestOptions(method: Option[HttpMethod.Value] = None,
                          auth: Option[HttpAuth] = None, params: Map[String, Any] = Map.empty,
                          headers: HttpHeaders = HttpHeaders.empty,
                          data: Option[HttpBody[_]] = None
                         ) extends Options with RequestOptionsOps {

  override type OptionType = RequestOptions

  override def lift(f: (RequestOptions) => RequestOptions) = f(this)

  override def toMap = Map("method" -> method, "auth" -> auth, "params" -> Some(params))
}


trait PaginationOptionsOps {
  type OptionType

  protected def liftPagination(f: PaginationOptions => PaginationOptions): OptionType

  def withPage(strategy: PaginationStrategy): OptionType = liftPagination(_.copy(strategy = Some(strategy)))

  def withPage(f: PaginationData => Typed): OptionType = withPage(PaginationStrategy(f))

  def limit(amount: Int) = liftPagination(_.copy(limit = Some(amount)))


}

case class PaginationOptions(strategy: Option[PaginationStrategy] = None, limit: Option[Int] = None)
  extends Options with PaginationOptionsOps {

  override type OptionType = PaginationOptions

  override def liftPagination(f: (PaginationOptions) => PaginationOptions) = f(this)


  override def toMap = Map("page" -> strategy.map(_.value), "page_limit" -> limit)
}

case class HttpOptions(
                        timeout: Option[Int] = None,
                        reattempts: Option[Int] = None,
                        redirect: Option[Int] = None,
                        verify: Option[Boolean] = None,
                        resultFormat: Option[ResultFormat.Kind] = None,
                        request: RequestOptions = RequestOptions(),
                        pagination: PaginationOptions = PaginationOptions()
                      ) extends Options with RequestOptionsOps with PaginationOptionsOps {

  lazy val mapValue = Map("timeout" -> timeout, "reattempts" -> reattempts,
    "redirect" -> redirect, "verify" -> verify, "result_format" -> resultFormat
  )

  override def toMap = (mapValue /: request.toMap)(_ + _)

  def withTimeout(amount: Int) = copy(timeout = Some(amount))

  def withReattempts(amount: Int) = copy(reattempts = Some(amount))

  def withRedirect(amount: Int) = copy(redirect = Some(amount))

  def withVerify(value: Boolean) = copy(verify = Some(value))

  def asJson = copy(resultFormat = Some(ResultFormat.Json))

  def asJsonp = copy(resultFormat = Some(ResultFormat.Jsonp))

  def asText = copy(resultFormat = Some(ResultFormat.Text))


  override type OptionType = HttpOptions

  override def lift(f: (RequestOptions) => RequestOptions) = copy(request = f(request))

  override def liftPagination(f: (PaginationOptions) => PaginationOptions) = copy(pagination = f(pagination))
}



