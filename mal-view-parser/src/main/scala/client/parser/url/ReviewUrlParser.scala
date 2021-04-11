package client.parser.url

import data.Link

trait ReviewUrlParser extends BaseUrlParser {
  protected def reviewParser: Parser[Link] = detail | helpful | voted | manga | anime

  private def anime = "/reviews.php?t=anime" ^^ {
    f1 => Link("Review-Anime", s"$f1", s"$f1")
  }

  private def manga = "/reviews.php?t=manga" ^^ {
    f1 => Link("Review-Manga", s"$f1", s"$f1")
  }

  private def voted = "/reviews.php?st=bestvoted" ^^ {
    f1 => Link("Review-Voted", s"$f1", s"$f1")
  }

  private def helpful = "/reviews.php?st=mosthelpful" ^^ {
    f1 => Link("Review-Helpful", s"$f1", s"$f1")
  }

  private def detail = "/reviews.php?id=" ~ ?("review_id") ^^ {
    case f1 ~ n1 => Link("Review-Detail", s"$f1${n1.view}", s"$f1${n1.origin}")
  }
}
