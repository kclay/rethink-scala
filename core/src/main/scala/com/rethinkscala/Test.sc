

//import com.rethinkscala._
//import com.rethinkscala.reflect.Reflector

/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 3/30/14
 * Time: 8:50 AM
 *
 */

/*
val json = """[
             |    {"id": 2, "player": "Bob", "points": 15, "type": "ranked"},
             |    {"id": 5, "player": "Alice", "points": 7, "type": "free"},
             |    {"id": 11, "player": "Bob", "points": 10, "type": "free"},
             |    {"id": 12, "player": "Alice", "points": 2, "type": "free"}
             |]""".stripMargin

val seq = Expr(Reflector.fromJson[Seq[Map[String, Any]]](json))



seq.group("player")
*/


trait FA[T]{
  def apply():Any
}

class ProduceRaw[T]
class Sequence[T] extends ProduceRaw[T]


class Pluck extends Sequence[Any]
implicit def toFa[T](implicit mf:Manifest[T]) = new FA[Sequence[T]]{
  def apply = mf
}

def foo[P](f: Any => ProduceRaw[P])(implicit fa:FA[P]) = fa()


foo((x:Any)=> new Sequence[Any])
//foo((x:Any)=>new Pluck)


