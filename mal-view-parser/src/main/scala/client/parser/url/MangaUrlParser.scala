package client.parser.url

import data.Link

trait MangaUrlParser extends BaseUrlParser {
  protected def mangaParser: Parser[Link] =
    top | search | more | pictures | clubs | features | forum | news |
      characters | stats | recommendations | reviews | details

  private def details = "/manga" ~ /("manga_id") ~ /("manga_name") ^^ {
    case f1 ~ v1 ~ v2 => Link("Manga-Details", s"$f1${v1.view}${v2.view}", s"$f1${v1.origin}${v2.origin}")
  }

  private def reviews = "/manga" ~ /("manga_id") ~ /("manga_name") ~ "/reviews" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Manga-Reviews", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def recommendations = "/manga" ~ /("manga_id") ~ /("manga_name") ~ "/userrecs" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Manga-Recommendations", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def stats = "/manga" ~ /("manga_id") ~ /("manga_name") ~ "/stats" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Manga-Stats", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def characters = "/manga" ~ /("manga_id") ~ /("manga_name") ~ "/characters" ~ opt("#staff") ^^ {
    case f1 ~ v1 ~ v2 ~ f2 ~ _ => Link("Manga-Characters", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def news = "/manga" ~ /("manga_id") ~ /("manga_name") ~ "/news" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Manga-News", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def forum = "/manga" ~ /("manga_id") ~ /("manga_name") ~ "/forum" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Manga-Forum", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def features = "/manga" ~ /("manga_id") ~ /("manga_name") ~ "/featured" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Manga-Features", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def clubs = "/manga" ~ /("manga_id") ~ /("manga_name") ~ "/clubs" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Manga-Clubs", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def pictures = "/manga" ~ /("manga_id") ~ /("manga_name") ~ "/pics" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Manga-Pictures", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def more = "/manga" ~ /("manga_id") ~ /("manga_name") ~ "/moreinfo" ^^ {
    case f1 ~ v1 ~ v2 ~ f2 => Link("Manga-More", s"$f1${v1.view}${v2.view}$f2", s"$f1${v1.origin}${v2.origin}$f2")
  }

  private def search = "/manga.php" ~ opt("?" ~ ".*".r) ^^ {
    case f1 ~ _ => Link("Manga-Search", s"$f1", s"$f1")
  }

  private def top = "/topmanga.php" ~ opt("?" ~ ".*".r) ^^ {
    case f1 ~ _ => Link("Manga-Top", s"$f1", s"$f1")
  }
}
