/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 9/12/13
 * Time: 2:59 PM
 */
import com.rethinkscala.ast.Binary;
import com.rethinkscala.ast.Var;
import com.rethinkscala.japi.BooleanFunction;
import  com.rethinkscala.r;
public class ApiTest {


    public void test(){

          BooleanFunction f = new BooleanFunction(){

              @Override
              public Binary apply(Var param) throws Exception {
                  return null;  //To change body of implemented methods use File | Settings | File Templates.
              }
          };

        r.branch(f)
    }
}
