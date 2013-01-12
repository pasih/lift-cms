package fi.paranoid.model

import net.liftweb.mongodb.record.{MongoMetaRecord, MongoRecord}
import net.liftweb.mongodb.record.field.{ObjectIdRefField, ObjectIdPk}
import net.liftweb.record.field._
import net.liftweb.common._
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.http._
import net.liftweb.util.{FieldContainer, Html5, FieldError}
import net.liftweb.record.field.StringField
import net.liftweb.common.Full

import xml.NodeSeq
import org.bson.types.ObjectId

class CustomContent private() extends MongoRecord[CustomContent] with ObjectIdPk[CustomContent] with Logger {
  def meta = CustomContent

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

  object parent extends ObjectIdRefField(this, CustomContent) {
    override def optional_? = true
  }

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
    CustomContent.findContent(identifier, "pages") match {
      case Full(p) if p.id.is == this.id.is => Nil // don't error on an update to this page
      case Full(p) => FieldError(this.identifier, errorMsg) :: Nil
      case _ => Nil
    }
}

object CustomContent extends CustomContent with MongoMetaRecord[CustomContent] with Logger {
  def findContentById(id: String): Box[CustomContent] =
    id match {
      case "addnew:page" =>
        Full(ContentLocHelper.NullCustomContent)
      case _ =>
        try {
          CustomContent.find(new ObjectId(id))
        } catch {
          case _: IllegalArgumentException => Empty
        }
    }

  // TODO: Check aspect == home or pages
  def findContent(identifier: String, aspect: String) =
    CustomContent.find("identifier" -> identifier)

  def findAllRootItems =
    CustomContent.findAll(("parent" -> ("$exists" -> false)))

  def findAllChildItems(parent: CustomContent) =
    CustomContent.findAll(("parent") -> parent.id.is)

  def setParent(p: CustomContent) = parent(p.id.is)
}

object ContentLocHelper extends Logger {

  def boxObjectId(id: String) =
    try {
      Full(new ObjectId(id))
    } catch {
      case _: IllegalArgumentException => Empty
    }

  lazy val item = CustomContent.createRecord
    .title("")
    .contents("")
    .aspect("pages")

  def NullCustomContent = item
  var root: CustomContent = null

  def ensureRoot() {
    root = CustomContent.find(("root") -> true) openOr {
      info("No root page found. Adding root page.")
      CustomContent.createRecord.aspect("home").title("home").identifier("root").root(true).save
    }
  }
}