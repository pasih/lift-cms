package fi.paranoid.snippet

import net.liftweb._
import http._
import common._
import fi.paranoid.model.{ContentLocHelper, ContentPage}
import fi.paranoid.config.MenuGroups
import xml.NodeSeq
import net.liftweb.util.Helpers._
import fi.paranoid.lib.{AdminNotification, ContentTreeWalker, ContentTreeItemEntryRenderer}
import xml.Text

/* TODO:
  Allow reparenting to upper level
 */
class AdminListPages extends Logger with AdminNotification {

  trait Direction
  case class DirUp() extends Direction
  case class DirDown() extends Direction

  private def forNonRootPage(page: ContentPage, f: => NodeSeq) =
    page.root.is.getOrElse(false) match {
      case true => NodeSeq.Empty
      case false => f
    }

  private def deletePage(page: ContentPage) {
    S.notice(S ? "Page '%s' deleted.".format(page.title.is))
    showEvent("#u deleted page '%s'".format(page.title.is), updateTreeView = true)
    ContentPage.deletePage(page)
  }

  private def moveUp(page: ContentPage, f: => NodeSeq) =
    page.ordering.is match {
      case 0 => NodeSeq.Empty
      case _ => f
    }

  private def moveDown(page: ContentPage, f: => NodeSeq) = {
    page.parent.obj match {
      case Full(p) =>
        val n = ContentPage.countChildren(p)
        if (page.ordering.is == n - 1) NodeSeq.Empty else f
      case _ =>
        NodeSeq.Empty
    }
  }

  private def doReorder(page: ContentPage, direction: Direction) {
    val posLookupDir = direction match {
      case DirUp() => ((pos: Long) => (pos - 1))
      case DirDown() => ((pos: Long) => (pos + 1))
    }
    if (page.parent.obj.isEmpty) {
      warn("Warning, doReorder() empty parent")
      return
    }
    val pos1: Long = page.ordering.is

    ContentPage.findByPosition(posLookupDir(pos1), page.parent.obj.open_!) match {
      case Full(p2) =>
        p2.ordering(pos1).save
        page.ordering(posLookupDir(pos1)).save
        showEvent("#u reordered pages ('%s' and '%s')".format(p2.title.is, page.title.is), updateTreeView = true)
        S.notice("Page order updated.")
      case _ => warn("Warning, no previous item.")
    }
  }

  class AdminViewRenderer extends ContentTreeItemEntryRenderer {

    override def wrapMain(f: => NodeSeq): NodeSeq = <ul class="admin-page-list">{f}</ul>

    override def transformEntry(entry: ContentPage, in: NodeSeq): NodeSeq = {
      ContentPage.findAllInOrder.map(a => (a, a.title.is))
        (".editLink [href]" #> MenuGroups.editPage.createLink(Full(entry), Empty)  &
         ".order" #> entry.ordering.is.toString &
         // ".visitLink [href]" #> entry.linkTo &
         ".deleteLink" #> forNonRootPage(entry,
           SHtml.link("", () => ( deletePage(entry) ), Text(S ? "Delete"))) &
         ".addSubPage [href]" #> MenuGroups.editPage.createLink(Empty, Full(entry.id.is.toString)) &
         ".pageTitle" #> entry.title.is &
         ".moveUp" #> forNonRootPage(entry, moveUp(entry,
           SHtml.link("", () => ( doReorder(entry, DirUp()) ), Text(S ? "Move up")))) &
         ".moveDown" #> forNonRootPage(entry, moveDown(entry,
           SHtml.link("", () => ( doReorder(entry, DirDown()) ), Text(S ? "Move down"))))
        ).apply(in)
    }

    /* TODO: Move to a template */
    override def wrapChild(f: => NodeSeq): NodeSeq =
      <li class="item">
        Page: <span class="pageTitle">Page 1</span> (<span class="order">-1</span>)
        (<a href="#" class="visitLink">Show</a>)
        <div class="btn-group">
          <a class="btn editLink" href="#">Edit</a>
          <button class="btn deleteLink" onclick="return confirm('Are you sure?')" href="/">Delete</button>
          <a class="btn addSubPage" href="#">Add child page</a>
          <a class="btn moveUp" href="#">Move up</a>
          <a class="btn moveDown" href="#">Move down</a>
        </div>
        <div class="selectParent"></div>
      </li>
  }

  def render =
    SHtml.idMemoize(outer => {
      /* TODO */
      val walker = new ContentTreeWalker(ContentLocHelper.root, new AdminViewRenderer)
      "div" #> walker.traverse()
    })
}

