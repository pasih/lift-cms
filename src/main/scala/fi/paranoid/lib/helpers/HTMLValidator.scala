package fi.paranoid.lib.helpers

import org.owasp.html._


trait HtmlValidator {
  def validate(html: String): String
}

object HtmlValidatorService {
  def validator: HtmlValidator = new OwaspHtmlValidator
}

class OwaspHtmlValidator extends HtmlValidator {
  private val sanitizer = Sanitizers.FORMATTING and Sanitizers.BLOCKS and Sanitizers.IMAGES  and Sanitizers.LINKS and Sanitizers.STYLES

  def validate(html: String) =
    sanitizer.sanitize(html)
}


