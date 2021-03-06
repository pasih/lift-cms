package fi.paranoid.model

import net.liftweb.mongodb.record.{BsonMetaRecord, BsonRecord, MongoMetaRecord, MongoRecord}
import net.liftweb.mongodb.record.field.{BsonRecordListField, ObjectIdRefField, ObjectIdPk}
import net.liftweb.record.field._
import net.liftweb.common._
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.http.S
import net.liftweb.util.{BaseField, FieldContainer, Html5, FieldError}
import net.liftweb.record.field.StringField
import net.liftweb.common.{Full, Box}
import xml.NodeSeq
import org.bson.types.ObjectId

class ContentFragment private() extends BsonRecord[ContentFragment] {
  def meta = ContentFragment

  object fragmentName extends StringField(this, 512)
  object contents extends TextareaField(this, 1048576) {
    override def displayName = fragmentName.is

  }
}

object ContentFragment extends ContentFragment with BsonMetaRecord[ContentFragment]

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

  object template extends StringField(this, 512)
  object contentFragments extends BsonRecordListField(this, ContentFragment)

  object publishAt extends DateTimeField(this)

  object parent extends ObjectIdRefField(this, ContentPage) {
    override def optional_? = true
  }

  object ordering extends LongField(this)

  object showInMenu extends BooleanField(this, true)

  object root extends OptionalBooleanField(this)

  def editScreenFields = new FieldContainer {
    def allFields = List(title, identifier, showInMenu)
  }

  /* TODO: This should really only validate uniqueness amongst its parent's
    children */
  def uniqueIdentifier(errorMsg: ⇒ String)(identifier: String): List[FieldError] =
    ContentPage.findContent(identifier) match {
      case Full(p) if p.id.is == this.id.is => Nil // don't error on an update to this page
      case Full(p) => FieldError(this.identifier, errorMsg) :: Nil
      case _ => Nil
    }

  // TODO: fix page order
  def setParent(p: ContentPage) {
    parent(p.id.is)
  }

  object FragmentFields extends FieldContainer {
    def allFields: Seq[BaseField] = {
      contentFragments.is.map(fragment =>
        fragment.contents
      )
    }
  }

  def fragmentFields = FragmentFields
}

object ContentPage extends ContentPage with MongoMetaRecord[ContentPage] with Logger {

  def findContentById(id: ObjectId): Box[ContentPage] =
    ContentPage.find(id)

  def findContentById(id: String): Box[ContentPage] =
    try {
      ContentPage.find(new ObjectId(id))
    } catch {
      case _: IllegalArgumentException => Empty
    }

  def findContent(identifier: String) =
    ContentPage.find("identifier" -> identifier)

  def ltSort(a: ContentPage, b: ContentPage) =
    a.ordering.is < b.ordering.is

  def findAllInOrder =
  ContentPage.findAll.sortWith(ltSort)

  def findAllRootItems =
    ContentPage.findAll(("parent" -> ("$exists" -> false)))

  def findAllChildItems(parent: ContentPage) =
    ContentPage.findAll(("parent") -> parent.id.is).sortWith(ltSort)

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
    parent.headOption.foreach (
      ContentPage.findAllChildItems(_).foldLeft(0)((a, b) => { b.ordering(a).save; a + 1 })
    )
  }
}

object ContentLocHelper extends Logger {

  def boxObjectId(id: String) =
    try {
      Full(new ObjectId(id))
    } catch {
      case _: IllegalArgumentException => Empty
    }

  lazy val item = ContentPage.createRecord.title("")

  def NullCustomContent = item

  var root: ContentPage = null

  def ensureRoot() {
    root = ContentPage.find(("root") -> true) openOr {
      info("No root page found. Adding root page.")
      ContentPage.createRecord.title("home").identifier("root").template("default").root(true).save
    }
  }
}
