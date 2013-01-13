
package fi.paranoid.lib

import fi.paranoid.model.{ContentLocHelper, ContentPage}
import net.liftweb.common._
import net.liftweb.util._
import Helpers._
import net.liftweb.http._
import net.liftweb.sitemap._
import xml.{Text, NodeSeq}

class BaseContentLoc(val name: String, _aspect: String) extends Loc[ContentPage] with Logger {
  val BaseAspect = _aspect

  override def defaultValue = Full(ContentLocHelper.NullCustomContent)

  override def childValues = ContentPage.findAllChildItems(ContentLocHelper.root)

  def foo(e: ContentPage): List[MenuItem] =
    for {
      p <- ContentPage.findAllChildItems(e)
      l <- link.createLink(p).map(appendQueryParams(p))
    } yield MenuItem(text.text(p), l, foo(p),
      (currentValue openOr ContentLocHelper.NullCustomContent).identifier.is == p.identifier.is,
      false,
      allParams.flatMap {
        case v: Loc.LocInfo[_] => List(v())
        case _ => Nil
      })

  /* TODO: fix page equality test */
  override def supplimentalKidMenuItems: List[MenuItem] =
    foo(ContentLocHelper.root)

  def params = List(Loc.PlaceHolder, Loc.Template(myTemplate), Loc.LocGroup("topbar"))

  def myTemplate() =
    <lift:surround with="default" at="content"><lift:display /></lift:surround>

  /**
   * Generate a link based on the current page
   */
  val link =
    new Loc.Link[ContentPage](List(BaseAspect), false) {
      override def createLink(in: ContentPage) = {
        Full(Text("/"+urlEncode(BaseAspect)+"/"+urlEncode(in.identifier.is)))
      }
    }

  /**
   * What's the text of the link?
   */
  val text = new Loc.LinkText(calcLinkText _)

  def calcLinkText(in: ContentPage): NodeSeq = {
    if (in.title.is.length > 0) Text(in.title.is) else Text(name)
  }

  object Finder {
    def unapply(page: String): Option[ContentPage] =
      ContentPage.findContent(page, BaseAspect)
  }

  /**
   * Rewrite the request and emit the type-safe parameter
   */
  override val rewrite: LocRewrite =
    Full({
      case RewriteRequest(ParsePath(BaseAspect :: Finder(content) :: Nil,
      _, _, _), _, _) =>
        (RewriteResponse(BaseAspect :: Nil), content)
    })

  /**
   * Check for page-specific snippets and
   * do appropriate dispatching
   */
  override val snippets: SnippetTest = {
    case ("display", Full(v)) =>  display(v) _
  }

  def display(v: ContentPage)(in: NodeSeq) =
    v.content match {
      case Failure(msg, exception, chain) => <h1>TODO: Print error page</h1>
      case Empty => in // TODO
      case Full(x) => x
    }
}