package client.parser.url

import data.Link

trait ForbiddenUrlParser extends BaseUrlParser {
  protected def forbiddenParser: Parser[Link] = db_change

  private def db_change = "/dbchanges.php" ~ opt("?" ~ ".*".r) ^^ {
    case f1 ~ _ => Link("Forbidden-DB-Change", s"$f1", s"$f1", forbidden = true)
  }
}
