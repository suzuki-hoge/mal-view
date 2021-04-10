import client.Client
import data.Link
import generator.MermaidGenerator
import global.Log

object Main extends App {
  try {
    val page = Client.getPage(Link.top)
    MermaidGenerator(page)
  } finally {
    Log.logger.close()
  }
}
