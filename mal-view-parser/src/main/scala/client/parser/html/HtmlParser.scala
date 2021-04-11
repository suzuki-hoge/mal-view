package client.parser.html

import client.parser.url.UrlParser
import data.Link
import global.{Config, Log}

object HtmlParser {
  def parse(id: String, origin: Seq[String]): Seq[Link] = {
    val trimmed = origin.dropWhile(header).takeWhile(footer)

    val lines = if (trimmed.isEmpty) origin else trimmed

    val parsed = lines
      .flatMap(line => line.replaceAll("href", "\nhref").replaceAll("data-url", "\ndata-url").split("\n"))
      .filter(line => line.contains("href") || line.contains("data-url"))
      .filter(line => line.contains(s"https://${Config.domain}"))
      .map { line =>
        val regex(url) = line
          .replace(s"https://${Config.domain}", "")
          .replace("data-url", "href")
          .replace("&amp;", "&")
        UrlParser(url)
      }

    val links = parsed.filter(_.isRight).map(_.right.get).distinctBy(_.id).sortBy(_.path)

    links.map(s => s"https://${Config.domain}${s.origin}").foreach(Log.logger.success(id, _))
    parsed.filter(_.isLeft).map(_.left.get).distinct.sorted.map(s => s"https://${Config.domain}$s").foreach(Log.logger.failure(id, _))

    links
  }

  private val regex = """.*href\s*=\s*"([^"]*)".*""".r

  private def header(s: String): Boolean = !s.contains("""id="contentWrapper"""")

  private def footer(s: String): Boolean = !s.contains("""id="footer-block"""")

}
