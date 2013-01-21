package fi.paranoid.snippet
import fi.paranoid.lib.currentNode
import net.liftweb.http.S
import net.liftweb.common.Full
import net.liftweb.common.Logger
import net.liftweb.util.Html5
import xml.NodeSeq

class Content extends Logger {
  def render(in: NodeSeq): NodeSeq = {
    val node = currentNode.is
    info("Looking for page fragment " + S.attr("areaId").getOrElse("<undefined fragment>"))
    S.attr("areaId") match {
      case Full(f) =>
        val fragment = for {
          fragment <- node.page.contentFragments.is.find(_.fragmentName.is == f)
        } yield fragment
        fragment match  {
          case Some(a) =>
            Html5.parse("<div>" + a.contents.is + "</div>") openOr <h1>Invalid content.</h1>
          case _ => <h1>Could not find fragment '{ f }'</h1>
        }

      case _ => <h1>Error, no content fragment defined.</h1>
    }
  }
}
