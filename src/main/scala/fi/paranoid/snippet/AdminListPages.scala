package fi.paranoid.snippet

import net.liftweb._
import http._
import common._
import fi.paranoid.model.{ContentLocHelper, CustomContent}
import fi.paranoid.config.MenuGroups
import xml.{NodeSeq, Text}
import net.liftweb.util.Helpers._
import fi.paranoid.lib.{ContentTreeWalker, ContentTreeItemEntryRenderer, BaseContentLoc}
import xml.Text


class AdminListPages extends Logger {

  def deleteLink(page: CustomContent): NodeSeq =
    page.root.is.getOrElse(false)  match {
      case true => NodeSeq.Empty
      case false => SHtml.link("", () => ( page.delete_! ), Text(S ? "Delete"))
    }

  class AdminViewRenderer extends ContentTreeItemEntryRenderer {

    override def wrapMain(f: => NodeSeq): NodeSeq = <ul class="admin-page-list">{f}</ul>

    override def transformEntry(entry: CustomContent, in: NodeSeq): NodeSeq = {
      (".editLink [href]" #> MenuGroups.editPage.createLink(Full(entry), Empty)  &
        ".visitLink [href]" #> entry.linkTo &
        ".deleteLink" #> deleteLink(entry) &
        ".addSubPage [href]" #> MenuGroups.editPage.createLink(Empty, Full(entry.id.is.toString)) &
        ".pageTitle" #> entry.title.is).apply(in)
    }

    /* TODO: Move to a template */
    override def wrapChild(f: => NodeSeq): NodeSeq =
      <li class="item">
        Page: <span class="pageTitle">Page 1</span>
        (<a href="#" class="visitLink">Show</a>)
        <div class="btn-group">
          <a class="btn editLink" href="#">Edit</a>
          <button class="btn deleteLink" onclick="return confirm('Are you sure?')" href="/">Delete</button>
          <a class="btn addSubPage" href="#">Add child page</a>
        </div>
      </li>
  }

  def render = {
    val walker = new ContentTreeWalker(ContentLocHelper.root, new AdminViewRenderer {})
    walker.traverse()
  }
}
