package client.parser.url

import data.Link

trait ForumUrlParser extends BaseUrlParser {
  protected def forumParser: Parser[Link] = detail | list

  private def list = "/forum/" ~ opt(".*".r) ^^ {
    case f1 ~ _ => Link("Forum-List", s"$f1", s"$f1")
  }

  private def detail = "/forum/?topicid=" ~ ?("topic_id") ^^ {
    case f1 ~ v1 => Link("Forum-Detail", s"$f1${v1.view}", s"$f1${v1.origin}")
  }

  private def search = "/forum/search" ^^ {
    f1 => Link("Forum-Search", s"$f1", s"$f1")
  }
}
