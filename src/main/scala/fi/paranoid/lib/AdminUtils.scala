package fi.paranoid.lib

import fi.paranoid.model.User
import fi.paranoid.comet.AdminViewServer
import net.liftweb.common.{Logger, Empty, Full}

trait AdminNotification extends Logger {

  def showEvent(msg: String, updateTreeView: Boolean = false) {
    (User.currentUser: @unchecked) match {
      case Full(u) => AdminViewServer !  msg.replaceAll("#u", u.username.is)
      case Empty => warn("showEvent with no current user.")
    }
  }
}