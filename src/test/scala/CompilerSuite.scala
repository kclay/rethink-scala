import com.rethinkdb.Ast.Table
import org.scalatest.FunSuite

/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 3/20/13
 * Time: 8:19 PM 
 */
class CompilerSuite extends FunSuite {

  import com.rethinkdb._
  import com.rethinkdb.Ast._

  test("table insert compiler") {
    Table("tbl", true) insert (Seq(
      Map("id" -> 0),
      Map("id" -> 1)
    ))
  }
}
