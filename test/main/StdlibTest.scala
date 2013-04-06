package main

import org.scalatest.FunSuite
import scala.io.Source

// this test is obsolete
class StdlibTest extends FunSuite {

  test("stdlib test") {  
    assert(Joosc.compile(StdlibFiles.stdlibFiles) == main.Joosc.errCodeSuccess)
  }
}
