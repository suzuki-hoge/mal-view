package client.parser.url

import data.Link

trait CharacterUrlParser extends BaseUrlParser {
  protected def characterParser: Parser[Link] = detail | list

  private def list = "/character.php" ~ opt("?_location=" ~ "[a-z_]+".r) ^^ {
    case f1 ~ _ => Link("Character-List", s"$f1", s"$f1")
  }

  private def detail = "/character" ~ n("character_id") ~ s("character_name") ^^ {
    case f1 ~ n1 ~ s1 => Link("Character-Detail", s"$f1${n1.view}${s1.view}", s"$f1${n1.origin}${s1.origin}")
  }
}
