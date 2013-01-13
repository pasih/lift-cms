package fi.paranoid.snippet

import net.liftweb._
import http._
import common._
import fi.paranoid.model.{ContentLocHelper, CustomContent}
import fi.paranoid.config.MenuGroups
import xml.NodeSeq
import net.liftweb.util.Helpers._
import fi.paranoid.lib.{AdminNotification, ContentTreeWalker, ContentTreeItemEntryRenderer}
import xml.Text

object outerHolder extends RequestVar[Box[IdMemoizeTransform]](Empty)

class AdminListPages extends Logger with AdminNotification {

  trait Direction
  case class DirUp() extends Direction
  case class DirDown() extends Direction

  private def forNonRootPage(page: CustomContent, f: => NodeSeq) =
    page.root.is.getOrElse(false) match {
      case true => NodeSeq.Empty
      case false => f
    }

  private def deletePage(page: CustomContent) {
    S.notice(S ? "Page '%s' deleted.".format(page.title.is))
    showEvent("#u deleted page '%s'".format(page.title.is), updateTreeView = true)
    CustomContent.deletePage(page)
  }

  private def moveUp(page: CustomContent, f: => NodeSeq) =
    page.ordering.is match {
      case 0 => NodeSeq.Empty
      case _ => f
    }

  private def moveDown(page: CustomContent, f: => NodeSeq) = {
    val n = CustomContent.countChildren(page.parent.obj.open_!) // TODO: open_!
    warn("Page has " + n + " children.")
    if (page.ordering.is == n - 1)
      NodeSeq.Empty
    else
      f
  }

  private def doReorder(page: CustomContent, direction: Direction) {
    val posLookupDir = direction match {
      case DirUp() => ((pos: Long) => (pos - 1))
      case DirDown() => ((pos: Long) => (pos + 1))
    }
    if (page.parent.obj.isEmpty) {
      warn("Warning, doReorder() empty parent")
      return
    }
    val pos1: Long = page.ordering.is
    val page2Box = CustomContent.findByPosition(posLookupDir(pos1), page.parent.obj.open_!)
    if (page2Box.isEmpty) {
      warn("Warning, no previous item.")
      return
    }
    val page2 = page2Box.open_!
    page2.ordering(pos1).save
    page.ordering(posLookupDir(pos1)).save
    showEvent("#u reordered pages ('%s' and '%s')".format(page2.title.is, page.title.is), updateTreeView = true)
    S.notice("Page order updated.")
  }

  class AdminViewRenderer extends ContentTreeItemEntryRenderer {

    override def wrapMain(f: => NodeSeq): NodeSeq = <ul class="admin-page-list">{f}</ul>

    override def transformEntry(entry: CustomContent, in: NodeSeq): NodeSeq = {
      CustomContent.findAllInOrder.map(a => (a, a.title.is))
        (".editLink [href]" #> MenuGroups.editPage.createLink(Full(entry), Empty)  &
         ".order" #> entry.ordering.is.toString &
         ".visitLink [href]" #> entry.linkTo &
         ".deleteLink" #> forNonRootPage(entry,
           SHtml.link("", () => ( deletePage(entry) ), Text(S ? "Delete"))) &
         ".addSubPage [href]" #> MenuGroups.editPage.createLink(Empty, Full(entry.id.is.toString)) &
         ".pageTitle" #> entry.title.is &
         ".moveUp" #> forNonRootPage(entry, moveUp(entry,
           SHtml.link("", () => ( doReorder(entry, DirUp()) ), Text(S ? "Move up")))) &
         ".moveDown" #> forNonRootPage(entry, moveDown(entry,
           SHtml.link("", () => ( doReorder(entry, DirDown()) ), Text(S ? "Move down"))))
  //        ".selectParent" #> forNonRootPage(entry,
  //          SHtml.selectObj[CustomContent](allpages, Empty, (p: CustomContent) => {  println(p.title) })
  //        )
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
      val walker = new ContentTreeWalker(ContentLocHelper.root, new AdminViewRenderer)
      "div" #> walker.traverse()
    })
}

