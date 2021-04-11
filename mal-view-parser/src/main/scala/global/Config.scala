package global

object Config {
  val depth: Int = 2

  val domain: String = "myanimelist.net"
//  val domain: String = "dev10.al.myanimelist.net" // todo: basic auth

  val mocked: Boolean = true

  val mocks: String = s"./mocks"

  val log: String = s"../mal-view-viewer/generated/parsed.log"

  val out: String = s"../mal-view-viewer/generated/parsed.json"
}
