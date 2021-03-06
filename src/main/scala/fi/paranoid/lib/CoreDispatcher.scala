package fi.paranoid.lib

import net.liftweb.http.rest.RestHelper
import net.liftweb.http._
import net.liftweb.common.{Box, Logger}

object currentNode extends RequestVar[CmsNode](null)

object CoreDispatcher extends RestHelper with Logger {

  object Finder {
    def unapply(str: List[String]): Option[CmsNode] =
      CmsStructure.lookup(str)
  }

  def doPageDispatch(node: CmsNode, r: Req): Box[LiftResponse] = {
    currentNode(node)
    info("Looking up page '%s'".format(node.page.title.is))
    for {
      session <- S.session
      response <- session.processTemplate(Templates(List("cms-templates", node.page.template.is)),
        r, ParsePath(List("fooz"), "", absolute = false, endSlash = false), 200)
    } yield response
  }

  serve {
    case r @ Req(Finder(node), _, _) =>
      doPageDispatch(node, r)
  }
}
