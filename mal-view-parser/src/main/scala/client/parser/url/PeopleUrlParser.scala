package client.parser.url

import data.Link

trait PeopleUrlParser extends BaseUrlParser {
  protected def peopleParser: Parser[Link] = detail | list

  private def list = "/people.php" ~ opt("?_location=" ~ "[a-z_]+".r) ^^ {
    case f1 ~ _ => Link("People-List", s"$f1", s"$f1")
  }

  private def detail = "/people" ~ /("people_id") ~ /("people_name") ^^ {
    case f1 ~ v1 ~ v2 => Link("People-Detail", s"$f1${v1.view}${v2.view}", s"$f1${v1.origin}${v2.origin}")
  }
}
