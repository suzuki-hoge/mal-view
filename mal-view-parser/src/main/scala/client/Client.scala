package client

import client.fetcher.Fetcher
import client.parser.html.HtmlParser
import data.{Link, Page}
import global.{Config, Log}

import scala.collection.mutable

object Client {
  private var fetched = 0

  def getPage(link: Link, depth: Int = 1): Page = Page(link, getSubLinks(link, depth).map(getPage(_, depth + 1)))

  private val memo: mutable.Map[String, Seq[Link]] = mutable.Map.empty

  private def getSubLinks(link: Link, depth: Int): Seq[Link] = {
    print(s"fetching [ ${link.path} ]...")
    if (link.forbidden) Fetcher.forbidden(link).map(_.asInstanceOf[Link])
    else if (Config.depth < depth) Fetcher.tooDeep(link).map(_.asInstanceOf[Link])
    else if (memo.contains(link.id)) memoized(link)
    else remote(link)
  }

  private def memoized(link: Link): Seq[Link] = {
    println(" stopped ( memoized )")
    Log.logger.info(link.id, "fetching is stopped ( memoized )")
    memo(link.id)
  }

  private def remote(link: Link): Seq[Link] = {
    val links = HtmlParser.parse(link.id, Fetcher.fetch(link))
    fetched += 1
    memo += link.id -> links
    println(s" done ( $fetched )")
    links
  }
}
