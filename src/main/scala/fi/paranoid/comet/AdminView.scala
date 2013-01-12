package fi.paranoid.comet

import net.liftweb._
import common.{Logger, Box, Empty}
import http._
import util._
import collection.mutable
import fi.paranoid.config.SiteUtils
import fi.paranoid.snippet.AdminListPages

object outerHolder extends RequestVar[Box[IdMemoizeTransform]](Empty)

class AdminView extends CometActor with CometListener with Logger {
  private var msgs: mutable.IndexedSeq[EventInfo] = mutable.IndexedSeq()

  protected def registerWith = AdminViewServer

  override def lowPriority = {
    case v: mutable.IndexedSeq[EventInfo] => msgs = v; reRender()
  }

  def render = {
    SHtml.ajaxInvoke(() => outerHolder.is.foreach(a => {
        a.setHtml()
    }))
    "li *" #> msgs.reverse.map(m =>
      ".time" #> ("(" + m.at.toString(SiteUtils.dateFormat) + ")") &
        ".event" #> m.msg
    ) & ClearClearable
  }
}
