package client.fetcher

import data.Link
import global.{Config, Log}

import scala.io.Source

object Fetcher {
  def forbidden(link: Link): Seq[String] = {
    println(" stopped ( forbidden )")
    Log.logger.info(link.id, "fetching is stopped ( forbidden )")
    Seq()
  }

  def tooDeep(link: Link): Seq[String] = {
    println(" stopped ( too deep )")
    Log.logger.info(link.id, "fetching is stopped ( too deep )")
    Seq()
  }

  def fetch(link: Link): Seq[String] = {
    Log.logger.info(link.id, "fetched")
    if (Config.mocked) fromMock(link) else fromWeb(link)
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

    if (link.id == "Top") local(s"${Config.mocks}/top.html")
    else if (link.id == "Anime-Details") local(s"${Config.mocks}/anime-detail.html")
    else if (link.id == "Info-Genre") local(s"${Config.mocks}/info-genre.html")
    else Seq()
  }
}
