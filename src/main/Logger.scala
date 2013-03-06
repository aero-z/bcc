package main

object Logger {
  val debugEnabled = true
  def debug(a: Any) = if (debugEnabled) println(a)
}
