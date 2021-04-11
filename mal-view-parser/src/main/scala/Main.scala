import client.Client
import data.Link
import generator.HierarchicalEdgePrinter
import global.Log

object Main extends App {
  try {
    val page = Client.getPage(Link.top)
    HierarchicalEdgePrinter(page)
  } finally {
    Log.logger.close()
  }
}
