package parser.url

import data.Color.Gray
import data.Link

trait AboutUrlParser extends BaseUrlParser {
  protected def aboutParser: Parser[Link] =
    terms_of_use | sitemap | privacy_policy | notice_at_collection |
      cookie_policy | team | support | contact | top

  private def top = "/about.php" ~ opt("?_location=" ~ "[a-z_]+".r) ^^ {
    case f1 ~ _ => Link("About-Top", s"$f1", s"$f1", Gray, recursive = false)
  }

  private def contact = "/about.php?go=contact" ~ opt("&_location=" ~ "[a-z_]+".r) ^^ {
    case f1 ~ _ => Link("About-Contract", s"$f1", s"$f1", Gray, recursive = false)
  }

  private def support = "/about.php?go=support" ~ opt("&_location=" ~ "[a-z_]+".r) ^^ {
    case f1 ~ _ => Link("About-Support", s"$f1", s"$f1", Gray, recursive = false)
  }

  private def team = "/about.php?go=team" ~ opt("&_location=" ~ "[a-z_]+".r) ^^ {
    case f1 ~ _ => Link("About-Team", s"$f1", s"$f1", Gray, recursive = false)
  }

  private def cookie_policy = "/about" ~ "/cookie_policy" ^^ {
    case f1 ~ f2 => Link("About-Cookie-policy", s"$f1$f2", s"$f1$f2", Gray, recursive = false)
  }

  private def notice_at_collection = "/about" ~ "/notice_at_collection" ^^ {
    case f1 ~ f2 => Link("About-Notice-At-Collection", s"$f1$f2", s"$f1$f2", Gray, recursive = false)
  }

  private def privacy_policy = "/about" ~ "/privacy_policy" ^^ {
    case f1 ~ f2 => Link("About-Privacy-Policy", s"$f1$f2", s"$f1$f2", Gray, recursive = false)
  }

  private def sitemap = "/about" ~ "/sitemap" ^^ {
    case f1 ~ f2 => Link("About-Sitemap", s"$f1$f2", s"$f1$f2", Gray, recursive = false)
  }

  private def terms_of_use = "/about" ~ "/terms_of_use" ^^ {
    case f1 ~ f2 => Link("About-Terms-Of-Use", s"$f1$f2", s"$f1$f2", Gray, recursive = false)
  }
}
