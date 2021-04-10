package client.parser.url

import data.Link

import scala.util.parsing.combinator.JavaTokenParsers

trait BaseUrlParser extends JavaTokenParsers {
  protected def topParser: Parser[Link] = "/" ^^ (_ => Link.top)

  protected def n(view: String): Parser[Identifier] = "/[0-9]+".r ^^ (n => Identifier(s"/{$view}", n))

  protected def s(view: String): Parser[Identifier] = "/[a-zA-Z0-9_-]+".r ^^ (s => Identifier(s"/{$view}", s))
}
