package fi.paranoid
package snippet

import scala.xml.{NodeSeq, Null, Text}

import net.liftweb._
import common._
import http._
import http.js._
import http.js.JsCmds._
import http.js.JE._
import json._
import json.JsonDSL._
import util.{CssSel, Helpers}

object Notices extends Factory with Loggable {
  /*
   * Config
   */
  val errorTitle = new FactoryMaker[Box[String]](Empty){}
  val warningTitle = new FactoryMaker[Box[String]](Empty){}
  val noticeTitle = new FactoryMaker[Box[String]](Empty){}

  /**
    * Render notices
    */
  def render(html: NodeSeq): NodeSeq = {
    // Capture the value for later AJAX updates
    // TODO: Doesn't work with Ajax as RequestVar
    ShowAllNotices(Helpers.toBoolean(S.attr("showAll") or S.attr("showall")))

    <div id={LiftRules.noticesContainerId}>{renderNotices()}</div>
  }

  def renderNotices(): NodeSeq = {
    // Determine which formatting function to use based on tag usage
    val notices =
      if (ShowAllNotices.is) S.messages _
      else S.noIdMessages _

    // Compute the formatted set of messages for a given input
    def computeMessageDiv(args: (List[(NodeSeq, Box[String])], NoticeType.Value)): NodeSeq = args match {
      case (messages, noticeType) =>
        // Compute the resulting div
        notices(messages) match {
          case Nil => NodeSeq.Empty
          case msg :: Nil =>
            <div id={noticeType.id} class={cssCls(noticeType)} data-dismiss="alert">
              <a class="close" href="#">&times;</a>
              <p>{noticeTitle(noticeType).map(t => <strong>{t}</strong>).openOr(Text(""))} {msg}</p>
            </div>
          case msgs =>
            <div id={noticeType.id} class={cssCls(noticeType)} data-dismiss="alert">
              {noticeTitle(noticeType).map(t => <strong>{t}</strong>).openOr(Text(""))}
              <a class="close" href="#">&times;</a>
              { msgs.flatMap(e => { <p>{e}</p> }) }
            </div>
        }
    }

    // Render all three types together
    List((S.errors, NoticeType.Error),
         (S.warnings, NoticeType.Warning),
         (S.notices, NoticeType.Notice)).flatMap(computeMessageDiv)
  }

  /**
    * Render a single id's notices
    */
  def id(html: NodeSeq): NodeSeq = {
    S.attr("id") match {
      case Full(id) => tailScript(clearJsCmd) ++ <span id={id} class="notices-container">{renderIdMsgs(id)}</span>
      case _ => NodeSeq.Empty
    }
  }

  def renderIdMsgs(id: String): NodeSeq = {
    val errs = S.messagesById(id)(S.errors)
    val warns = S.messagesById(id)(S.warnings)
    val notes = S.messagesById(id)(S.notices)

    val highestLevel: Box[NoticeType.Value] = (errs.length, warns.length, notes.length) match {
      case (e, _, _) if (e > 0) => Full(NoticeType.Error)
      case (_, w, _) if (w > 0) => Full(NoticeType.Warning)
      case (_, _, n) if (n > 0) => Full(NoticeType.Notice)
      case _ => Empty
    }

    <ul>
    {
      List(
        (errs, NoticeType.Error),
        (warns, NoticeType.Warning),
        (notes, NoticeType.Notice)
      ).flatMap { case (messages, noticeType) =>
        messages match {
           case Nil => NodeSeq.Empty
           case msgs =>
             msgs.flatMap(e => {
              <li class={"field-%s".format(lowerCaseTitle(noticeType))}>{e}</li>
            })
         }
      }
    }
    </ul> ++ tailScript(onErrorJsCmd(id, highestLevel))
  }

  def onErrorJsCmd(id: String, level: Box[NoticeType.Value]): JsCmd = level match {
    case Full(noticeType) => Run("""app.notices.onError("%s", "%s");""".format(id, lowerCaseTitle(noticeType)))
    case _ => Noop
  }

  def clearJsCmd: JsCmd = Call("""app.notices.clear""")

  private def lowerCaseTitle(noticeType: NoticeType.Value): String = noticeType match {
    case NoticeType.Notice => "info"
    case _ => noticeType.lowerCaseTitle
  }

  private def noticeTitle(noticeType: NoticeType.Value): Box[String] = noticeType match {
    case NoticeType.Notice => noticeTitle.vend
    case NoticeType.Error => errorTitle.vend
    case NoticeType.Warning => warningTitle.vend
  }

  private def cssCls(noticeType: NoticeType.Value): String = noticeType match {
    case NoticeType.Notice => "alert alert-info"
    case NoticeType.Error => "alert alert-error"
    case NoticeType.Warning => "alert alert-warning"
  }

  def tailScript(script: JsCmd): NodeSeq =
    Script(OnLoad(script))

  def init(): Unit = {
    LiftRules.noticesToJsCmd = () => {
      // Compute the global notices first
      val groupMessages: JsCmd = LiftRules.jsArtifacts.setHtml(LiftRules.noticesContainerId, renderNotices()) & clearJsCmd

      // We need to determine the full set of IDs that need messages rendered.
      val idSet: List[String] = (S.idMessages((S.errors)) ++
                   S.idMessages((S.warnings)) ++
                   S.idMessages((S.notices))).map(_._1).distinct

      // Merge each Id's messages and effects into the JsCmd chain
      idSet.foldLeft(groupMessages) {
        (chain,id) => chain &
          LiftRules.jsArtifacts.setHtml(id, renderIdMsgs(id))
      }
    }

    /**
      * LiftScreen overwrites the class on form labels and bootstrap
      * requires the control-label class. So, we disable LiftScreen's
      * overwriting of the class.
      */
    LiftScreenRules.messageStyles.default.set({ nt: NoticeType.Value => nt match {
      case NoticeType.Notice => Null
      case NoticeType.Warning => Null
      case NoticeType.Error => Null
    }})
  }
}

object ShowAllNotices extends SessionVar[Boolean](false)

