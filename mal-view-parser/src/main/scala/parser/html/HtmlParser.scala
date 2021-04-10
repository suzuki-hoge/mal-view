package parser.html

import data.{Link, Page}
import global.{Config, Log}
import parser.url.UrlParser

import scala.collection.mutable
import scala.io.Source

object HtmlParser {
  def apply(link: Link, depth: Int = 1): Page = Page(link, parseLinks(link.id, fetchLines(link, depth)).map(apply(_, depth + 1)))

  private val memo: mutable.Map[String, Seq[String]] = mutable.Map.empty

  private def fetchLines(link: Link, depth: Int): Seq[String] = {
    if (!link.recursive) {
      Log.logger.info(link.id, "fetching is stopped ( forbidden )")
      Seq()
    } else if (Config.depth < depth) {
      Log.logger.info(link.id, "fetching is stopped ( too deep )")
      Seq()
    } else if (memo.contains(link.id)) {
      Log.logger.info(link.id, "fetching is stopped ( memoized )")
      memo(link.id)
    } else {
      Log.logger.info(link.id, "fetched")

      val fetched = if (Config.mocked) fromMock(link) else fromWeb(link)

      memo += link.id -> fetched

      fetched
    }
  }

  private def fromWeb(link: Link): Seq[String] = {
    val s = Source.fromURL(s"https://${Config.domain}${link.origin}", "utf-8")
    val lines = s.getLines.toSeq
    s.close
    lines
  }

  private def fromMock(link: Link): Seq[String] = {
    def local(path: String) = {
      val s = Source.fromFile(path)
      val lines = s.getLines.toSeq
      s.close
      lines
    }

    if (link.id == "Top")
      local(s"${Config.mocks}/top.html")
    else if (link.id == "Anime-Details")
      local(s"${Config.mocks}/anime-detail.html")
    else if (link.id == "Info-Genre")
      local(s"${Config.mocks}/info-genre.html")
    else
      Seq()
  }

  private def parseLinks(id: String, origin: Seq[String]): Seq[Link] = {
    def header(s: String): Boolean = !s.contains("""id="contentWrapper"""")

    def footer(s: String): Boolean = !s.contains("""id="footer-block"""")

    val trimmed = origin.dropWhile(header).takeWhile(footer)
    val lines = if (trimmed.isEmpty) origin else trimmed

    val regex = """.*href\s*=\s*"([^"]*)".*""".r

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


    val subs = parsed.filter(_.isRight).map(_.right.get).distinctBy(_.id).sortBy(_.view)

    subs.map(s => s"https://${Config.domain}${s.origin}").foreach(Log.logger.success(id, _))
    parsed.filter(_.isLeft).map(_.left.get).distinct.sorted.map(s => s"https://${Config.domain}$s").foreach(Log.logger.failure(id, _))

    subs
  }

}
