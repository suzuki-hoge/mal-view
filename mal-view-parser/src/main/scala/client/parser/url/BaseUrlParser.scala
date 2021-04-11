package client.parser.url

import data.Link

import scala.util.parsing.combinator.JavaTokenParsers

trait BaseUrlParser extends JavaTokenParsers {
  protected def topParser: Parser[Link] = "/" ^^ {
    _ => Link.top
  }

  protected def /(view: String): Parser[Identifier] = ("/[0-9]+".r | "/[^/]+".r) ^^ {
    s => Identifier(s"/{$view}", s)
  }

  protected def ?(view: String): Parser[Identifier] = ("[0-9]+".r | "[^/]+".r) ^^ {
    s => Identifier(s"{$view}", s)
  }
}
