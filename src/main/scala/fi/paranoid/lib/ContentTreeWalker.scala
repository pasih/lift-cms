package fi.paranoid.lib

import fi.paranoid.model.CustomContent
import xml.{Text, NodeSeq}
import net.liftweb.common.Logger

trait ContentTreeItemEntryRenderer {
  def wrapChildren(f: => NodeSeq): NodeSeq = <ul>{f}</ul>

  def wrapMain(f: => NodeSeq): NodeSeq = <ul>{f}</ul>

  def wrapChild(f: => NodeSeq): NodeSeq = <li><span>{f}</span></li>

  def transformEntry(entry: CustomContent, in: NodeSeq): NodeSeq = {
    in
  }

  def render(entry: CustomContent, level: Int): NodeSeq =
    wrapMain(transformEntry(entry, wrapChild(Text(entry.title.is))))
}

class ContentTreeWalker(root: CustomContent, renderer: ContentTreeItemEntryRenderer) {

  private def renderItems(c: CustomContent, level: Int): NodeSeq = {
    var ns: NodeSeq = NodeSeq.Empty
    for (e <- CustomContent.findAllChildItems(c)) {
      ns ++= renderer.render(e, level)
      ns ++= renderer.wrapChildren(renderItems(e, level + 1))
    }
    ns
  }

  def traverse(): NodeSeq = {
    val level = 0
    renderer.render(root, level) ++ renderer.wrapChildren(renderItems(root, level + 1))
  }
}
