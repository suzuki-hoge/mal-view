package client.parser.url

import data.Link

trait NewsUrlParser extends BaseUrlParser {
  protected def newsParser: Parser[Link] = tag | detail | list

  private def tag = "/news/tag" ~ /("tag_name") ^^ {
    case f1 ~ v1 => Link("News-Tag", s"$f1${v1.view}", s"$f1${v1.origin}")
  }

  private def list = "/news" ~ opt(".*".r) ^^ {
    case f1 ~ _ => Link("News-List", s"$f1", s"$f1")
  }

  private def detail = "/news" ~ /("news_id") ^^ {
    case f1 ~ v1 => Link("News-Detail", s"$f1${v1.view}", s"$f1${v1.origin}")
  }
}
