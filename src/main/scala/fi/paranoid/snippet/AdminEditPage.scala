package fi.paranoid.snippet

import net.liftweb._
import common.{Empty, Failure, Full, Logger, Box}
import http._
import fi.paranoid.model.{ContentFragment, ContentLocHelper, ContentPage}
import net.liftweb.util.Helpers._
import fi.paranoid.lib.AdminNotification
import fi.paranoid.lib.helpers.HtmlValidatorService
import scala.xml._

class AdminEditPage(params: Box[(Box[ContentPage], Box[String])])
  extends LiftScreen with Logger with AdminNotification {

  var page: Box[ContentPage] = Empty
  var newPage = false
  var parentId: String = ""

  (params: @unchecked) match {
    case Full(a) =>
      page = a._1
      parentId = a._2 openOr ""
  }

  var editingPage = page match {
    case Full(p) => p
    case Failure(_, _, _) =>
      S.error("Page could not be edited!")
      S.redirectTo("/admin/")
    case Empty =>
      newPage = true
      ContentPage.createRecord.template("default")
  }
  object content extends ScreenVar(editingPage)

  // Ensure template fields are available
  // TODO: de-hardcode 'default' and warn if no template was found.
  val template = Templates(List("cms-templates", content.template.is))

  // Find out if we're calling the Content snippet and if so, return
  // the areaId param
  def areaId(item: String): Box[String] = {
    item.split("\\?") match {
      case Array("Content", b) =>
        b.split("=") match {
          case Array("areaId", id) => return Full(id)
          case _ => Empty
        }
      case _ => Empty
    }
    Empty
  }

  var fragmentsUsed: List[String] = List()
  template match {
    case Full(t) =>
      (t \\ "@class").foreach {
        // Find all snippet calls and check if there's an areaId call in there
        _.text.split(":").foreach( a =>
          areaId(a) match {
            case Full(f) => fragmentsUsed ::= f
            case _ =>
          })}
    case _ =>
      warn("Could not open a page template.")
  }

  // Remove all fragments not used by the template
  // TODO: move to Trash
  content.is.contentFragments(content.is.contentFragments.is.filter(fragment =>
    if (fragment.fragmentName.is != "" && fragmentsUsed.contains(fragment.fragmentName.is)) {
      fragmentsUsed = fragmentsUsed.filterNot(_ == fragment.fragmentName.is)
      true
    } else false
  ))
  // Add missing fragments
  fragmentsUsed.foreach(a =>
      content.is.contentFragments.atomicUpdate( ContentFragment.createRecord.fragmentName(a) :: _))

  val parentPage: Box[ContentPage] = ContentPage.findContentById(parentId.toString)

  override def screenTop = newPage match {
    case true =>
      <div class="info">Parent page:
        { (parentPage openOr ContentLocHelper.NullCustomContent).title.is }
      </div>
    case false =>
      <div>Parent page: { (content.is.parent.obj openOr ContentLocHelper.NullCustomContent).title.is } </div>
  }

  addFields(() => content.is.editScreenFields)
  addFields(() => content.is.fragmentFields)

  override val cancelButton = super.cancelButton % ("class" -> "btn") % ("tabindex" -> "2")
  override val finishButton = super.finishButton % ("class" -> "btn btn-primary") % ("tabindex" -> "1")

  override def allTemplatePath: List[String] = List("templates-hidden", "content-wizard")

  protected def finish() {
    val c = content.is
    parentPage match {
      case Full(x) =>
        c.parent(x.id.is)
        c.ordering(ContentPage.countChildren(x))
      case _ => // No-op
    }
    c.save
    S.notice("Page '" + c.title.is + "' added!")

    if (newPage)
      showEvent(S ? "#u added page: '%s'".format(c.title.is), updateTreeView = true)
    else
      showEvent(S ? "#u edited page: '%s'".format(c.title.is), updateTreeView = true)
    S.redirectTo("/admin/")
  }
}
