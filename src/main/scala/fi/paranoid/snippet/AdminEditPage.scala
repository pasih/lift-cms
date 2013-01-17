package fi.paranoid.snippet

import net.liftweb._
import common.{Empty, Failure, Full, Logger, Box}
import http._
import fi.paranoid.model.{ContentLocHelper, ContentPage}
import net.liftweb.util.Helpers._
import fi.paranoid.lib.AdminNotification
import fi.paranoid.lib.helpers.HtmlValidatorService


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
      ContentPage.createRecord
  }
  object content extends ScreenVar(editingPage)

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

  override val cancelButton = super.cancelButton % ("class" -> "btn") % ("tabindex" -> "1")
  override val finishButton = super.finishButton % ("class" -> "btn btn-primary") % ("tabindex" -> "2")

  override def allTemplatePath: List[String] = List("templates-hidden", "content-wizard")

  protected def finish() {
    val c = content.is
    parentPage match {
      case Full(x) =>
        c.parent(x.id.is)
        c.ordering(ContentPage.countChildren(x))
      case _ => // No-op
    }

    c.contents(HtmlValidatorService.validator.validate(c.contents.is))
    c.save
    S.notice("Page '" + c.title.is + "' added!")

    if (newPage)
      showEvent(S ? "#u added page: '%s'".format(c.title.is), updateTreeView = true)
    else
      showEvent(S ? "#u edited page: '%s'".format(c.title.is), updateTreeView = true)
    S.redirectTo("/admin/")
  }
}
