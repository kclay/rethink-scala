/**
 * Created with IntelliJ IDEA.
 * User: keyston
 * Date: 9/12/13
 * Time: 2:59 PM
 */


import com.rethinkscala.japi.Connection;
import com.rethinkscala.japi.Result;
import com.rethinkscala.japi.r;

import com.rethinkscala.net.Version;
import com.rethinkscala.net.Version2;


public class ApiTest {


    public void test() {


        Connection connection = new Connection(Version2.builder().build(), 5000);


        Result<String> result = connection.run(r.expr("hello").add(r.expr(1)));

          /*
          BooleanFunction f = new BooleanFunction(){

              @Override
              public Binary apply(Var param) throws Exception {
                  return null;  //To change body of implemented methods use File | Settings | File Templates.
              }
          };*/

        //  r.branch(f)
    }
}
