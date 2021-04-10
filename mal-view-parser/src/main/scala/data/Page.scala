package data

case class Page(self: Link, subs: Seq[Page]) {
  def traverse[T](f: Link => T): Seq[T] = List(f(self)) ++ subs.flatMap(sub => sub.traverse(f))

  def traverseWithSubs[T](f: (Link, Seq[Page]) => Seq[T]): Seq[T] = f(self, subs) ++ subs.flatMap(sub => sub.traverseWithSubs(f))
}
