package generator

import data.Color._
import data.{Color, Link, Page}
import global.Config

import java.io.PrintWriter

object MermaidGenerator {
  def apply(page: Page): Unit = {
    val definitions = page.traverse(definition).sorted.distinct
    val styles = page.traverse(style).sorted.distinct
    val clicks = page.traverse(click).sorted.distinct
    val relations = page.traverseWithSubs((self, subs) => subs.flatMap(sub => relation(self, sub.self))).sorted.distinct

    render(definitions ++ styles ++ clicks ++ relations)
  }

  private def definition(link: Link): String = s"""${link.id}("${link.id}<br><br>${link.view}")"""

  private def style(link: Link): String = s"style ${link.id} fill:#${rgb(link.color)}, stroke: #000000"

  private def rgb(color: Color): String = color match {
    case Red => "ffcccc"
    case Blue => "ccccff"
    case Green => "ccffcc"
    case Purple => "ffccff"
    case Yellow => "ffffcc"
    case Cyan => "219ddd"
    case Gray => "cccccc"
  }

  private def click(link: Link): String = s"""click ${link.id} "https://${Config.domain}${link.origin}""""

  private def relation(src: Link, dst: Link): Option[String] = if (src.id != dst.id) Some(s"${src.id} --> ${dst.id}") else None

  private def render(contents: Seq[String]): Unit = {
    val html = new PrintWriter(Config.out)

    val body = contents.distinct.map(s => s"    $s").mkString("\n")

    html.write(
      s"""<h2>${Config.domain} ( ${Config.depth} depth )</h2>
         |<div class="mermaid">
         |graph LR
         |$body
         |</div>
         |
         |<script src="https://unpkg.com/mermaid/dist/mermaid.min.js"></script>
         |""".stripMargin
    )

    html.close()
  }
}
