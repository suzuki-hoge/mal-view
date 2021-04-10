package global

import java.io.PrintWriter

case class Log(pw: PrintWriter) {
  def info(id: String, s: String): Unit = pw.append(s"[INFO] [id=$id] $s\n")

  def success(id: String, s: String): Unit = pw.append(s"[SUCCESS] [id=$id] $s\n")

  def failure(id: String, s: String): Unit = pw.append(s"[FAILURE] [id=$id] $s\n")

  def close(): Unit = pw.close()
}

object Log {
  val logger: Log = Log(new PrintWriter(Config.log))
}
