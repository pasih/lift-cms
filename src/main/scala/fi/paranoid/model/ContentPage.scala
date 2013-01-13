package fi.paranoid.model

import net.liftweb.mongodb.record.{MongoMetaRecord, MongoRecord}
import net.liftweb.mongodb.record.field.{ObjectIdRefField, ObjectIdPk}
import net.liftweb.record.field._
import net.liftweb.common._
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.http.S
import net.liftweb.util.{FieldContainer, Html5, FieldError}
import net.liftweb.record.field.StringField
import net.liftweb.common.Full
import xml.NodeSeq
import org.bson.types.ObjectId

class ContentPage private() extends MongoRecord[ContentPage] with ObjectIdPk[ContentPage] with Logger {
  def meta = ContentPage

  object title extends StringField(this, 512) {
    override def validations = valMinLen(1, S ? "Title cannot be blank") _ ::
      valMaxLen(512, S ? "Title is too long") _ ::
      super.validations
  }

  object identifier extends StringField(this, 512) {
    override def validations = valMinLen(1, S ? "Identifier cannot be blank") _ ::
      valMaxLen(512, S ? "Identifier is too long") _ ::
      uniqueIdentifier(S ? "Identifier has to be unique") _ ::
      super.validations
    override def setFilter = trim _ :: toLower _ :: super.setFilter
  }

  object contents extends TextareaField(this, 1048576)

  object aspect extends StringField(this, 512)

  object publishAt extends DateTimeField(this)

  object parent extends ObjectIdRefField(this, ContentPage) {
    override def optional_? = true
  }

  object ordering extends LongField(this)

  object root extends OptionalBooleanField(this)

  // TODO: cache
  def content: Box[NodeSeq] =
    if (contents.value.nonEmpty)
      Html5.parse("<div>" + contents.value + "</div>")
    else
      Empty

  def editScreenFields = new FieldContainer {
    def allFields = List(title, identifier, contents)
  }

  def linkTo = "/" + aspect.is + "/" + identifier.is

  def uniqueIdentifier(errorMsg: ⇒ String)(identifier: String): List[FieldError] =
    ContentPage.findContent(identifier, "pages") match {
      case Full(p) if p.id.is == this.id.is => Nil // don't error on an update to this page
      case Full(p) => FieldError(this.identifier, errorMsg) :: Nil
      case _ => Nil
    }
}

object ContentPage extends ContentPage with MongoMetaRecord[ContentPage] with Logger {
  def findContentById(id: String): Box[ContentPage] =
    id match {
      case "addnew:page" =>
        Full(ContentLocHelper.NullCustomContent)
      case _ =>
        try {
          ContentPage.find(new ObjectId(id))
        } catch {
          case _: IllegalArgumentException => Empty
        }
    }

  // TODO: Check aspect == home or pages
  def findContent(identifier: String, aspect: String) =
    ContentPage.find("identifier" -> identifier)

  def ltSort(a: ContentPage, b: ContentPage) =
    a.ordering.is < b.ordering.is

  def findAllInOrder =
  ContentPage.findAll.sortWith(ltSort)

  def findAllRootItems =
    ContentPage.findAll(("parent" -> ("$exists" -> false)))

  def findAllChildItems(parent: ContentPage) =
    ContentPage.findAll(("parent") -> parent.id.is).sortWith(ltSort)

  def setParent(p: ContentPage) = parent(p.id.is)

  def findByPosition(pos: Long, parent: ContentPage) =
    ContentPage.find(("ordering" -> pos) ~ ("parent" -> parent.id.is))

  def countChildren(page: ContentPage) =
    ContentPage.count("parent" -> page.id.is)

  def foreachChild(p: ContentPage, f: ContentPage => Unit) {
    ContentPage.findAllChildItems(p).foreach(a => {
      foreachChild(a, f)
      f(a)
    })
  }

  def deletePage(page: ContentPage) {
    def del(p: ContentPage) {
      warn("Deleting page: " + p.title.is)
      p.delete_!
    }
    ContentPage.foreachChild(page, del)
    warn("Deleting page: " + page.title.is)
    val parent = page.parent.obj
    page.delete_!
    if (!parent.isEmpty) {
      /* Reorder children */
      val children = ContentPage.findAllChildItems(parent.open_!)
      children.foldLeft(0)((a, b) => { b.ordering(a).save; a + 1 })
    }
  }
}

object ContentLocHelper extends Logger {

  def boxObjectId(id: String) =
    try {
      Full(new ObjectId(id))
    } catch {
      case _: IllegalArgumentException => Empty
    }

  lazy val item = ContentPage.createRecord
    .title("")
    .contents("")
    .aspect("pages")

  def NullCustomContent = item
  var root: ContentPage = null

  def ensureRoot() {
    root = ContentPage.find(("root") -> true) openOr {
      info("No root page found. Adding root page.")
      ContentPage.createRecord.aspect("home").title("home").identifier("root").root(true).save
    }
  }
}