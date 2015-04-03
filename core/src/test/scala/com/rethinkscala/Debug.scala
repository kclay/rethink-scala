package com.rethinkscala

import org.scalatest.FunSuite
import com.rethinkscala.Blocking.functional._
import ql2.Ql2


/**
 * Created by IntelliJ IDEA.
 * User: Keyston
 * Date: 4/2/2015
 * Time: 7:56 AM
 */


case class Participant(
                        id: String,
                        `type`: String,
                        name: String,
                        unreadCount: Int) extends Document

case class Message(
                    authorId: String,
                    authorType: String,
                    `type`: String,
                    content: String,
                    created: Long,
                    offerId: Option[String] = None,
                    sku: Option[String] = None,
                    price: Option[Double] = None,
                    txnId: Option[String] = None) extends Document

case class Conversation(
                         id: Option[String],
                         participants: Seq[Participant],
                         checkinId: String,
                         messages: Seq[Message],
                         status: String,
                         created: Long,
                         addedTo: Long,
                         hidden: Option[Boolean] = Some(false)) extends Document

class Debug extends FunSuite with WithBase {


  test("debug") {





    val convos = table.to[Conversation]
    val messages = Seq(
      Message(authorId = "54757e32712f1c24005881e2", authorType = "user", content = "Yet another conversation with this store and checkin - will it work?",
        created = 1425441210548L, `type` = "test")
    )
    val participants = Seq(
      Participant(id = "54757e32712f1c24005881e2", name = "mick", `type` = "user",
        unreadCount = 0), Participant(id = "546557ad49c1ca1900c1f4b7", name = "Industrie Bankstwon", `type` = "store", unreadCount = 1)
    )
    val convo = Conversation(id = None, addedTo = 1425441210548L, checkinId = "54759d24712f1c09a8a76dd3",
      created = 1425441210548L, messages = messages, participants = participants,
      status = "normal")
    val updated = convos.insert(convo).run.flatMap {
      res =>
        val id = res.generatedKeys.head
        val newParts = List(Participant("54757e32712f1c24005881e2", "user", "mick", 0),
          Participant("546557ad49c1ca1900c1f4b7", "store", "Industrie Bankstwon", 0))
        convos.get(id)
          .update(Map("participants" -> newParts)).run
    }
   println(updated)

  }
}