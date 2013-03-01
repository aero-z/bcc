package main

object Logger {
  val debugEnabled = false
  def debug(a: Any) = if (debugEnabled) println(a)
}
