package fi.paranoid.model

import net.liftweb.http.{Templates, LiftRules}
import java.io.File
import net.liftweb.common.{Box, Empty, Full, Logger}

/*
  PageTemplates includes the tools to manage CMS-specific templates
  (i.e. the templates within CmsTemplatesDir). Content snippet calls
  within those pages adds user content into the page.
 */
object PageTemplates extends Logger {
  val CmsTemplatesDir = "cms-templates"

  // TODO: Move this somewhere more accessible :)
  private def nullToBox[T](x: T) =
    if (x == null)
      Empty
    else
      Full(x)

  /* List all templates in the CMS templates directory */
  def listAvailable: List[String] = {
    val templateUrl = LiftRules.context.resource("/" + CmsTemplatesDir)

    if (templateUrl == null) {
      warn("Template directory not found!")
      return List("")
    }
    nullToBox(new File(templateUrl.toURI).list()) match {
      case Full(z) => z.toList.filter(_.endsWith(".html")).map(a => a.take(a.length - 5))
      case _ => List("")
    }
  }

  // Find out if we're calling the Content snippet and if so, return
  // the areaId param
  private def areaId(item: String): Box[String] = {
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

  def listContentFragments(template: String) = {
    var fragmentsUsed: List[String] = List()
    Templates(List(CmsTemplatesDir, template)) match {
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
    fragmentsUsed
  }
}