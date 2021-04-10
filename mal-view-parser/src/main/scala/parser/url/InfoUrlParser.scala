package parser.url

import data.Color.Cyan
import data.Link

trait InfoUrlParser extends BaseUrlParser {
  protected def infoParser: Parser[Link] = genre

  private def genre = "/info.php?go=genre" ^^ (
    f1 => Link("Info-Genre", s"$f1", s"$f1", Cyan)
    )
}
