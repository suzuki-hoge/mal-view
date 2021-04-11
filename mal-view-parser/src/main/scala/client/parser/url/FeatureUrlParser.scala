package client.parser.url

import data.Link

trait FeatureUrlParser extends BaseUrlParser {
  protected def featureParser: Parser[Link] = columnists | tags | detail | list

  private def columnists = "/featured/columnist" ^^ {
    f1 => Link("Feature-Columnists", s"$f1", s"$f1")
  }

  private def tags = "/featured/tag" ^^ {
    f1 => Link("Feature-Tags", s"$f1", s"$f1")
  }

  private def list = "/featured" ~ opt(".*".r) ^^ {
    case f1 ~ _ => Link("Feature-List", s"$f1", s"$f1")
  }

  private def detail = "/featured" ~ /("anime_id") ~ /("anime_name") ^^ {
    case f1 ~ v1 ~ v2 => Link("Feature-Detail", s"$f1${v1.view}${v2.view}", s"$f1${v1.origin}${v2.origin}")
  }
}
