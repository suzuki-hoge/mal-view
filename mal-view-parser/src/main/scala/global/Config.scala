package global

object Config {
  val depth: Int = 2

  val domain: String = "myanimelist.net"
//  val domain: String = "dev10.al.myanimelist.net" with basic auth

  val mocked: Boolean = true

  val mocks: String = s"./mocks"

  val log: String = s"./generated/mal-view.log"

  val out: String = s"./generated/mal-view.html"
}
