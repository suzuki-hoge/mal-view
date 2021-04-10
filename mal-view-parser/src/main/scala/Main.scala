import data.Link
import global.Log
import parser.html.HtmlParser
import generator.MermaidGenerator

object Main extends App {
  val page = HtmlParser(Link.top)

  MermaidGenerator(page)

  Log.logger.close()
}
