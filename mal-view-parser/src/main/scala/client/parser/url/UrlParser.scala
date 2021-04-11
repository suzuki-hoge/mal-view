package client.parser.url

import data.Link

object UrlParser
  extends AnimeUrlParser
    with MangaUrlParser
    with CharacterUrlParser
    with AboutUrlParser
    with PeopleUrlParser
    with FeatureUrlParser
    with ForumUrlParser
    with NewsUrlParser
    with ReviewUrlParser
    with RecommendationUrlParser
    with ForbiddenUrlParser
    with ProfileUrlParser
    with InfoUrlParser {
  def apply(url: String): Either[String, Link] = parseAll(parser, url) match {
    case Success(result, next) => Right(result)
    case NoSuccess(error, next) => Left(url)
  }

  private def parser: Parser[Link] =
    infoParser | profileParser | forbiddenParser | recommendationParser | reviewParser |
      newsParser | forumParser | featureParser | peopleParser |
      aboutParser | characterParser | mangaParser | animeParser | topParser
}
