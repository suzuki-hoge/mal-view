package client.parser.url

import data.Link

trait MyList extends BaseUrlParser {
  protected def myListParser: Parser[Link] =
    anime | manga

  private def anime = "/animelist" ~ /("user_id") ~ opt(".*".r) ^^ {
    case f1 ~ v1 ~ _ => Link("MyList-Anime", s"$f1${v1.view}", s"$f1${v1.origin}")
  }

  private def manga = "/mangalist" ~ /("user_id") ~ opt(".*".r) ^^ {
    case f1 ~ v1 ~ _ => Link("MyList-Manga", s"$f1${v1.view}", s"$f1${v1.origin}")
  }
}
