package fi.paranoid.lib

import net.liftweb.http.rest.RestHelper
import net.liftweb.http._
import net.liftweb.common.{Full, Empty, Box, Logger}
import net.liftweb.util.Helpers._
import xml.Unparsed


object CoreDispatcher extends RestHelper with Logger {

  object Finder {
    def unapply(str: List[String]): Option[CmsNode] =
      CmsStructure.lookup(str)
  }

  // TODO: Move to an actual template
  def template =
    <lift:surround with="default" at="content"><lift:display /></lift:surround>

  def doPageDispatch(node: CmsNode, r: Req): Box[LiftResponse] = {
    val template = Full(<lift:surround with="default" at="content">
      { Unparsed(node.content.open_!.toString()) }
      </lift:surround>)

    for {
      session <- S.session
      response <- session.processTemplate(template, r, ParsePath(List("fooz"), // TODO
        "", absolute = false, endSlash = false), 200)
    } yield response
  }

  serve {
    case r @ Req(Finder(data), _, _) =>
      doPageDispatch(data, r)
  }
}
