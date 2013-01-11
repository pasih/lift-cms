package fi.paranoid.snippet

import net.liftweb._
import http._
import common._
import util.Helpers._
import fi.paranoid.model.CustomContent
import sitemap._

case class ParamInfo(theParam: String)


// a snippet that takes the page parameter information
class ShowParam(pi: ParamInfo)  {
  def render = "*" #> pi.theParam
}

class TestSnippet {
  // Create a menu for /param/somedata
  val menu = Menu.param[ParamInfo]("Param", "Param",
    s => Full(ParamInfo(s)),
    pi => pi.theParam) / "admin" / "param" / **
  lazy val loc = menu.toLoc

  def render = "*" #> loc.currentValue.map(_.theParam)
}