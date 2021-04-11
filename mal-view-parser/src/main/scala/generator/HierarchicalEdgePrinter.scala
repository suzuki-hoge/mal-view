package generator

import data.{Link, Page}
import global.Config

import java.io.PrintWriter

object HierarchicalEdgePrinter {
  def apply(page: Page): Unit = {
    val elements = page.traverse(p => List(p) ++ p.subs)
      .groupBy(p => p.self.id)
      .map(selectOne)
      .values.flatten.toSeq
      .map(p => (p.self.id, path(p.self), s"https://${Config.domain}${p.self.origin}", p.subs.map(sub => path(sub.self))))
      .sortBy(t => t._1)
      .map(e => s"""{ "id": ${surround(e._1)}, "path": ${surround(e._2)}, "origin": ${surround(e._3)}, "subs": [${e._4.map(surround).mkString(", ")}] }""")

    val json = new PrintWriter(Config.out)
    json.write(s"""[\n${elements.mkString(",\n")}\n]""")
    json.close()
  }

  private def selectOne(pair: (String, Seq[Page])): (String, Seq[Page]) = pair match {
    case (k, v) =>
      if (v.length == 1) (k, v)
      else if (v.exists(_.subs.nonEmpty)) (k, v.find(_.subs.nonEmpty).toSeq)
      else (k, v.headOption.toSeq)
  }

  private def path(link: Link): String = s"""root:${link.id.toLowerCase.split("-").init.mkString("-").replace("-", ":")}:${link.path}"""

  private def surround(s: String): String = s""""$s""""
}
