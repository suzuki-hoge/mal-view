package data

import data.Color.Green

case class Link(id: String, view: String, origin: String, color: Color, recursive: Boolean = true)

object Link {
  val top: Link = Link("Top", "/", "/", Green)
}