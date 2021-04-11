package data

case class Page(self: Link, subs: Seq[Page]) {
  def traverse[T](f: Page => Seq[Page]): Seq[Page] = f(this) ++ subs.flatMap(_.traverse(f))
}
