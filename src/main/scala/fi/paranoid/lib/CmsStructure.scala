package fi.paranoid.lib

import fi.paranoid.model.{ContentLocHelper, ContentPage}
import net.liftweb.common._
import org.bson.types.ObjectId


// TODO: Look up pages by ID rather than the human-readable identifier.
// We need it for numerous things (ID updates, pages with same ID etc.)
class CmsNode(private var p: ContentPage) extends Logger {
  val objectId = p.id.is
  val identifier = p.identifier.is
  private var refresh = false

  var parent: CmsNode = null

  var children: List[CmsNode] = List()

  def addChild(node: CmsNode) = {
    node.parent = this
    children ::= node
    info("Adding node %s (parent: %s)".format(node.identifier, this.identifier))
    node
  }

  def markDirty() { refresh = true }

  def page = {
    if (refresh) {
      val update = ContentPage.findContentById(objectId)
      update match {
        case Full(u) => info("Successfully refreshed page %s".format(identifier)); p = u
        case _ => warn("Failed to refresh page from database!")
      }
      refresh = false
    }
    p
  }

  def hasChildById(objectId: ObjectId) =
    children.find(_.objectId == objectId)

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

  def update(page: ContentPage) {
    findByObjectId(page.id.is) match {
      // If this page is available, mark it dirty for a refresh from the DB.
      case Some(n) =>
        n.markDirty()
      // If the page is not available, find its parent and add it.
      case None =>
        page.parent.obj.foreach ( a =>
          findByObjectId(a.id.is) match {
            case Some(parent) =>
              parent.addChild(new CmsNode(page))
            case _ => error("CmsStructure update() failed for: %s".format(page.identifier.is))
          }
        )
    }
  }

  def findByObjectId(id: ObjectId) =
    if (id == root.objectId)
      Some(root)
    else
      root.hasChildById(id)

  def findByIdentifier(identifier: String) = {
    if (identifier == root.identifier)
      Some(root)
    else
      root.hasChild(identifier)
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
