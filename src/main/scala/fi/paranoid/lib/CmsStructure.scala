package fi.paranoid.lib

import fi.paranoid.model.{ContentLocHelper, ContentPage}
import net.liftweb.common._

class CmsNode(val page: ContentPage) extends Logger {
  private val identifier = page.identifier.is

  var parent: CmsNode = null

  var children: List[CmsNode] = List()

  def addChild(node: CmsNode) = {
    node.parent = this
    children ::= node
    info("Adding node %s (parent: %s)".format(node.identifier, this.identifier))
    node
  }

  def hasChild(identifier: String) =
    children.find(_.identifier == identifier)
}

object CmsStructure extends Logger {
  private var root: CmsNode = null

  def lookup(url: List[String]): Option[CmsNode] = {
    val urlParts = if (url.last == "index") url.init else url
    if (urlParts.isEmpty)
      return Some(root)

    root.hasChild(urlParts.head) match {
      // Short-circuit first level
      case Some(c) if (urlParts.length == 1) =>
        Some(c)
      case Some(c) =>
        var elem: Option[CmsNode] = Some(c)
        urlParts.tail.foreach(a => elem = elem.map(_.hasChild(a)) getOrElse None)
        elem
      case None => None
    }
  }

  def load() {
    def addSubPages(n: CmsNode) {
      for (p <- ContentPage.findAllChildItems(n.page))
        addSubPages(n.addChild(new CmsNode(p)))
    }
    root = new CmsNode(ContentLocHelper.root)
    addSubPages(root)
  }
}
