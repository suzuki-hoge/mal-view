package client.parser.url

import data.Link

trait PeopleUrlParser extends BaseUrlParser {
  protected def peopleParser: Parser[Link] = detail | list

  private def list = "/people.php" ~ opt("?_location=" ~ "[a-z_]+".r) ^^ {
    case f1 ~ _ => Link("People-List", s"$f1", s"$f1")
  }

  private def detail = "/people" ~ n("people_id") ~ s("people_name") ^^ {
    case f1 ~ n1 ~ s1 => Link("People-Detail", s"$f1${n1.view}${s1.view}", s"$f1${n1.origin}${s1.origin}")
  }
}
