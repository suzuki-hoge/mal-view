package client.parser.url

import data.Link

trait ProfileUrlParser extends BaseUrlParser {
  protected def profileParser: Parser[Link] = reviews | recommendations | friends | clubs | detail

  private def detail = "/profile" ~ /("user_name") ^^ {
    case f1 ~ v1 => Link("Profile-Detail", s"$f1${v1.view}", s"$f1${v1.origin}")
  }

  private def clubs = "/profile" ~ /("user_name") ~ "clubs" ^^ {
    case f1 ~ v1 ~ f2 => Link("Profile-Clubs", s"$f1${v1.view}$f2", s"$f1${v1.origin}$f2")
  }

  private def friends = "/profile" ~ /("user_name") ~ "friends" ^^ {
    case f1 ~ v1 ~ f2 => Link("Profile-Friends", s"$f1${v1.view}$f2", s"$f1${v1.origin}$f2")
  }

  private def recommendations = "/profile" ~ /("user_name") ~ "recommendations" ^^ {
    case f1 ~ v1 ~ f2 => Link("Profile-Recommendations", s"$f1${v1.view}$f2", s"$f1${v1.origin}$f2")
  }

  private def reviews = "/profile" ~ /("user_name") ~ "reviews" ^^ {
    case f1 ~ v1 ~ f2 => Link("Profile-Reviews", s"$f1${v1.view}$f2", s"$f1${v1.origin}$f2")
  }
}
