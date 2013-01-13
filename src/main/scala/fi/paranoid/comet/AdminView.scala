package fi.paranoid.comet

import net.liftweb._
import common.{Full, Logger, Box, Empty}
import http._
import http.js.JsCmds.Replace
import util._
import collection.mutable
import fi.paranoid.config.SiteUtils

class AdminView extends CometActor with CometListener with Logger {
  private var msgs: mutable.IndexedSeq[EventInfo] = mutable.IndexedSeq()

  protected def registerWith = AdminViewServer

  override def lowPriority = {
    case v: mutable.IndexedSeq[EventInfo] => msgs = v; reRender()
  }

  def render = {
    "li *" #> msgs.reverse.map(m =>
      ".time" #> ("(" + m.at.toString(SiteUtils.dateFormat) + ")") &
        ".event" #> m.msg
    ) & ClearClearable
  }
}
