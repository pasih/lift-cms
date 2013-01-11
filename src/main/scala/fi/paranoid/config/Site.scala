package fi.paranoid
package config

import model.{ContentLocHelper, CustomContent, User}

import net.liftweb._
import common._
import http.{LiftRules, S}
import sitemap._
import sitemap.Loc._

import net.liftmodules.mongoauth.Locs
import lib.BaseContentLoc
import snippet.AdminEditPage

object MenuGroups extends Logger {
  val SettingsGroup = LocGroup("settings")
  val TopBarGroup = LocGroup("topbar")
  val AdminGroup = LocGroup("admin")
  val editPage = Menu.params[(Box[CustomContent], Box[String])]("Edit page", "Edit page",
      {
        case a :: b :: Nil =>
          Full((CustomContent.findContentById(a), Full(b)))
        case a :: Nil =>
          Full((CustomContent.findContentById(a), Empty))
        case _ =>
          Empty
      }, pp =>
        List((pp._1 openOr ContentLocHelper.NullCustomContent).id.is.toString, pp._2 openOr "")
    ) / "admin" / "test" / * / "parent" / *
}

/*
 * Wrapper for Menu locations
 */
case class MenuLoc(menu: Menu) {
  lazy val url: String = S.contextPath + menu.loc.calcDefaultHref
  lazy val fullUrl: String = S.hostAndPath + menu.loc.calcDefaultHref
}

object Site extends Locs {
  import MenuGroups._

  // locations (menu entries)
  val home = MenuLoc(Menu.i("Home") / "index" >> TopBarGroup)
  val loginToken = MenuLoc(buildLoginTokenMenu)
  val logout = MenuLoc(buildLogoutMenu)
  private val profileParamMenu = Menu.param[User]("User", "Profile",
    User.findByUsername _,
    _.username.is
  ) / "user" >> Loc.CalcValue(() => User.currentUser)
  lazy val profileLoc = profileParamMenu.toLoc

  val password = MenuLoc(Menu.i("Password") / "settings" / "password" >> RequireLoggedIn >> SettingsGroup)
  val account = MenuLoc(Menu.i("Account") / "settings" / "account" >> SettingsGroup >> RequireLoggedIn)
  val editProfile = MenuLoc(Menu("EditProfile", "Profile") / "settings" / "profile" >> SettingsGroup >> RequireLoggedIn)
  val register = MenuLoc(Menu.i("Register") / "register" >> RequireNotLoggedIn)

  private def menus = List(
    home.menu,
    Menu.i("Login") / "login" >> RequireNotLoggedIn,
    register.menu,
    loginToken.menu,
    logout.menu,
    profileParamMenu,
    account.menu,
    password.menu,
    editProfile.menu,
    Menu.i("About") / "about" >> TopBarGroup,
    Menu.i("Admin") / "admin" / "index" >> AdminGroup,
    MenuGroups.editPage,
    Menu.i("Throw") / "throw" >> Hidden,
    Menu.i("Error") / "error" >> Hidden,
    Menu.i("404") / "404" >> Hidden,
    Menu(new BaseContentLoc("HomePage", "pages"))
  )

  LiftRules.snippetDispatch.append {
    case "AdminEditPage" => new AdminEditPage(MenuGroups.editPage.currentValue)
  }

  /*
   * Return a SiteMap needed for Lift
   */
  def siteMap: SiteMap = SiteMap(menus:_*)
}
