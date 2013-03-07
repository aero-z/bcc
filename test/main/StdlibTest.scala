package main

import org.scalatest.FunSuite
import scala.io.Source

class StdlibTest extends FunSuite {

  test("stdlib test") {    
    assert(Joosc.check(StdlibFiles.stdlibFiles) == main.Joosc.errCodeSuccess)
  }
}
