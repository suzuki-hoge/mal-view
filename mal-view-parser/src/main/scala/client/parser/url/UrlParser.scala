package client.parser.url

import data.Link

object UrlParser
  extends AnimeUrlParser
    with CharacterUrlParser
    with AboutUrlParser
    with PeopleUrlParser
    with ForbiddenUrlParser
    with InfoUrlParser {
  def apply(url: String): Either[String, Link] = parseAll(parser, url) match {
    case Success(result, next) => Right(result)
    case NoSuccess(error, next) => Left(url)
  }

  private def parser: Parser[Link] = infoParser | forbiddenParser | peopleParser | characterParser | animeParser | aboutParser | topParser
}
