
import controllers.Implicits
import models.ConnectionPoolMsg
import org.apache.log4j.PropertyConfigurator
import play.api.{Application, GlobalSettings}
import models.ConnectionPoolMsg
import com.rethinkscala.Blocking._


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 11/6/2014
 * Time: 7:32 AM 
 */
object Global extends GlobalSettings {
  override def onStart(app: Application) = {
    import controllers.Implicits._
    PropertyConfigurator.configure("log4j.properties")
    val tbl = r.tableAs[ConnectionPoolMsg]("CheckInHitMsg")
    tbl.delete.run
    tbl.create.run
    val entries = for (i <- 0 to 10) yield ConnectionPoolMsg.create
    tbl.insert(entries).run
    super.onStart(app)
  }
}
