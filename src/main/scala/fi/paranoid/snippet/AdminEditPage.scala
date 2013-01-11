package fi.paranoid.snippet

import net.liftweb._
import common.{Empty, Failure, Full, Logger, Box}
import http._
import util.Helpers._
import fi.paranoid.model.{ContentLocHelper, CustomContent}
import java.util.Date
import fi.paranoid.config.MenuGroups
import org.bson.types.ObjectId
import xml.NodeSeq

class AdminEditPage(params: Box[(Box[CustomContent], Box[String])]) extends LiftScreen with Logger {
  var page: Box[CustomContent] = Empty

  var newPage = false

  var parentId: String = ""
  (params: @unchecked) match {
    case Full(a) =>
      page = a._1
      parentId = a._2 openOr ""
  }

  // TODO: Cleanup
  var editingPage = page match {
    case Full(p) => p
    case Failure(_, _, _) => /* TODO: Handle gracefully */
      S.error("Page could not be edited!")
      S.redirectTo("/admin/")
    case Empty =>
      newPage = true
      CustomContent.createRecord.aspect("pages")
  }
  object content extends ScreenVar(editingPage)

  val parentPage: Box[CustomContent] = CustomContent.findContentById(parentId.toString)
  warn(parentPage)

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
    c.aspect("pages")
    parentPage match {
      case Full(x) => c.parent(x.id.is)
      case _ => // No-op
    }
    c.save
    S.notice("Page '" + c.title.is + "' added!")
  }
}
