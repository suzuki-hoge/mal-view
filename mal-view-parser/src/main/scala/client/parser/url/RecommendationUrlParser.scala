package client.parser.url

import data.Link

trait RecommendationUrlParser extends BaseUrlParser {
  protected def recommendationParser: Parser[Link] = detail | manga | anime

  private def anime = "recommendations.php?s=recentrecs&t=anime" ^^ {
    f1 => Link("Recommendation-Anime", s"$f1", s"$f1")
  }

  private def manga = "recommendations.php?s=recentrecs&t=manga" ^^ {
    f1 => Link("Recommendation-Anime", s"$f1", s"$f1")
  }

  private def detail = "/recommendations/anime/" ~ ?("anime_id") ~ "-" ~ ?("anime_id") ^^ {
    case f1 ~ v1 ~ f2 ~ v2 => Link("Recommendation-Detail", s"$f1${v1.view}$f2${v2.view}", s"$f1${v1.origin}$f2${v2.origin}")
  }
}
