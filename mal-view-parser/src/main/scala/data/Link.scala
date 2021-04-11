package data

case class Link(id: String, path: String, origin: String, forbidden: Boolean = false)

object Link {
  val top: Link = Link("Top", "/", "/")
}