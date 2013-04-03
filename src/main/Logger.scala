package main

object Logger {
  var debugEnabled = false
  def debug(a: => Any) = if (debugEnabled) println(a)
}
