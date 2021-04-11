package client.parser.url

import data.Link

trait AnimeUrlParser extends BaseUrlParser {
  protected def animeParser: Parser[Link] =
    producer | top | seasonal | search | pictures | clubs | features | forum | news |
      characters | stats | recommendations | reviews | episode | episodes | videos | details

  private def details = "/anime" ~ /("anime_id") ~ /("anime_name") ^^ {
    case f1 ~ v1 ~ v2 => Link("Anime-Details", s"$f1${v1.view}${v2.view}", s"$f1${v1.origin}${v2.origin}")
  }

  private def videos = "/anime" ~ /("anime_id") ~ /("anime_name") ~ "/video" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Anime-Videos", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def episodes = "/anime" ~ /("anime_id") ~ /("anime_name") ~ "/episode" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Anime-Episodes", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def episode = "/anime" ~ /("anime_id") ~ /("anime_name") ~ "/episode" ~ /("episode_number") ^^ {
    case f1 ~ v1 ~ v2 ~ f2 ~ n2 => Link("Anime-Episode", s"$f1${v1.view}${v2.view}$f2${n2.view}", s"$f1${v1.origin}${v2.origin}$f2${n2.origin}")
  }

  private def reviews = "/anime" ~ /("anime_id") ~ /("anime_name") ~ "/reviews" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Anime-Reviews", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def recommendations = "/anime" ~ /("anime_id") ~ /("anime_name") ~ "/userrecs" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Anime-Recommendations", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def stats = "/anime" ~ /("anime_id") ~ /("anime_name") ~ "/stats" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Anime-Stats", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def characters = "/anime" ~ /("anime_id") ~ /("anime_name") ~ "/characters" ~ opt("#staff") ^^ {
    case f1 ~ v1 ~ v2 ~ f2 ~ _ => Link("Anime-Characters", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def news = "/anime" ~ /("anime_id") ~ /("anime_name") ~ "/news" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Anime-News", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def forum = "/anime" ~ /("anime_id") ~ /("anime_name") ~ "/forum" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Anime-Forum", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def features = "/anime" ~ /("anime_id") ~ /("anime_name") ~ "/featured" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Anime-Features", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def clubs = "/anime" ~ /("anime_id") ~ /("anime_name") ~ "/clubs" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Anime-Clubs", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def pictures = "/anime" ~ /("anime_id") ~ /("anime_name") ~ "/pics" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Anime-Pictures", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def search = "/anime.php" ~ opt("?" ~ ".*".r) ^^ {
    case f1 ~ _ => Link("Anime-Search", s"$f1", s"$f1")
  }

  private def seasonal = "/anime/season" ~ opt(/("yyyy") ~ /("season")) ^^ {
    case f1 ~ _ => Link("Anime-Season", s"$f1", s"$f1")
  }

  private def top = "/topanime.php" ~ opt("?" ~ ".*".r) ^^ {
    case f1 ~ _ => Link("Anime-Top", s"$f1", s"$f1")
  }

  private def producer = "/producer" ~ /("producer_id") ~ /("producer_name") ^^ {
    case f1 ~ v1 ~ v2 => Link("Anime-Producer", s"$f1${v1.view}${v2.view}", s"$f1${v1.origin}${v2.origin}")
  }
}
