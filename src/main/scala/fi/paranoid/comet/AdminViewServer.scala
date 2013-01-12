package fi.paranoid.comet

import net.liftweb._
import net.liftweb.common.Logger
import http._
import actor._
import collection.mutable
import common.{Empty, Box}
import org.joda.time.DateTime

case class EventInfo(msg: String) {
  val at = new DateTime
}

object AdminViewServer extends LiftActor with ListenerManager with Logger {
  private var msgs = mutable.IndexedSeq(EventInfo("Welcome."))

  def createUpdate = msgs

  override def lowPriority = {
    case e: EventInfo => msgs = msgs :+ e; updateListeners()
    case s: String => msgs = msgs :+ new EventInfo(s); updateListeners()
  }
}
