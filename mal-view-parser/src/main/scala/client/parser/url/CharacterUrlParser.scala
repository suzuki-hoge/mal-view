package client.parser.url

import data.Link

trait CharacterUrlParser extends BaseUrlParser {
  protected def characterParser: Parser[Link] = pictures | features | clubs | detail | list

  private def list = "/character.php" ~ opt("?" ~ ".*".r) ^^ {
    case f1 ~ _ => Link("Character-List", s"$f1", s"$f1")
  }

  private def detail = "/character" ~ /("character_id") ~ /("character_name") ^^ {
    case f1 ~ v1 ~ v2 => Link("Character-Detail", s"$f1${v1.view}${v2.view}", s"$f1${v1.origin}${v2.origin}")
  }

  private def clubs = "/character" ~ /("character_id") ~ /("character_name") ~ "/clubs" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Character-Club", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def features = "/character" ~ /("character_id") ~ /("character_name") ~ "/featured" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Character-Features", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def pictures = "/character" ~ /("character_id") ~ /("character_name") ~ "/pictures" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Character-Pictures", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }
}
