package client.parser.url

import data.Link

trait AnimeUrlParser extends BaseUrlParser {
  protected def animeParser: Parser[Link] =
    search | pictures | clubs | featured | forum | news |
      characters | stats | recommendations | reviews | episode | episodes | videos | details

  private def details = "/anime" ~ n("anime_id") ~ s("anime_name") ^^ {
    case f1 ~ n1 ~ s1 => Link("Anime-Details", s"$f1${n1.view}${s1.view}", s"$f1${n1.origin}${s1.origin}")
  }

  private def videos = "/anime" ~ n("anime_id") ~ s("anime_name") ~ "/video" ^^ {
    case f1 ~ n1 ~ s1 ~ f2 => Link("Anime-Videos", s"$f1${n1.view}${s1.view}", s"$f1${n1.origin}${s1.origin}$f2")
  }

  private def episodes = "/anime" ~ n("anime_id") ~ s("anime_name") ~ "/episode" ^^ {
    case f1 ~ n1 ~ s1 ~ f2 => Link("Anime-Episodes", s"$f1${n1.view}${s1.view}$f2", s"$f1${n1.origin}${s1.origin}$f2")
  }

  private def episode = "/anime" ~ n("anime_id") ~ s("anime_name") ~ "/episode" ~ n("episode_number") ^^ {
    case f1 ~ n1 ~ s1 ~ f2 ~ n2 => Link("Anime-Episode", s"$f1${n1.view}${s1.view}${n2.view}", s"$f1${n1.origin}${s1.origin}$f2${n2.origin}")
  }

  private def reviews = "/anime" ~ n("anime_id") ~ s("anime_name") ~ "/reviews" ^^ {
    case f1 ~ n1 ~ s1 ~ f2 => Link("Anime-Reviews", s"$f1${n1.view}${s1.view}$f2", s"$f1${n1.origin}${s1.origin}$f2")
  }

  private def recommendations = "/anime" ~ n("anime_id") ~ s("anime_name") ~ "/userrecs" ^^ {
    case f1 ~ n1 ~ s1 ~ f2 => Link("Anime-Recommendations", s"$f1${n1.view}${s1.view}$f2", s"$f1${n1.origin}${s1.origin}$f2")
  }

  private def stats = "/anime" ~ n("anime_id") ~ s("anime_name") ~ "/stats" ^^ {
    case f1 ~ n1 ~ s1 ~ f2 => Link("Anime-Stats", s"$f1${n1.view}${s1.view}$f2", s"$f1${n1.origin}${s1.origin}$f2")
  }

  private def characters = "/anime" ~ n("anime_id") ~ s("anime_name") ~ "/characters" ~ opt("#staff") ^^ {
    case f1 ~ n1 ~ s1 ~ f2 ~ _ => Link("Anime-Characters", s"$f1${n1.view}${s1.view}$f2", s"$f1${n1.origin}${s1.origin}$f2")
  }

  private def news = "/anime" ~ n("anime_id") ~ s("anime_name") ~ "/news" ^^ {
    case f1 ~ n1 ~ s1 ~ f2 => Link("Anime-News", s"$f1${n1.view}${s1.view}$f2", s"$f1${n1.origin}${s1.origin}$f2")
  }

  private def forum = "/anime" ~ n("anime_id") ~ s("anime_name") ~ "/forum" ^^ {
    case f1 ~ n1 ~ s1 ~ f2 => Link("Anime-Forum", s"$f1${n1.view}${s1.view}$f2", s"$f1${n1.origin}${s1.origin}$f2")
  }

  private def featured = "/anime" ~ n("anime_id") ~ s("anime_name") ~ "/featured" ^^ {
    case f1 ~ n1 ~ s1 ~ f2 => Link("Anime-Featured", s"$f1${n1.view}${s1.view}$f2", s"$f1${n1.origin}${s1.origin}$f2")
  }

  private def clubs = "/anime" ~ n("anime_id") ~ s("anime_name") ~ "/clubs" ^^ {
    case f1 ~ n1 ~ s1 ~ f2 => Link("Anime-Clubs", s"$f1${n1.view}${s1.view}$f2", s"$f1${n1.origin}${s1.origin}$f2")
  }

  private def pictures = "/anime" ~ n("anime_id") ~ s("anime_name") ~ "/pics" ^^ {
    case f1 ~ n1 ~ s1 ~ f2 => Link("Anime-Pictures", s"$f1${n1.view}${s1.view}$f2", s"$f1${n1.origin}${s1.origin}$f2")
  }

  private def search = "/anime.php" ~ opt("?" ~ ".*".r) ^^ {
    case f1 ~ _ => Link("Anime-Search", s"$f1", s"$f1")
  }
}
