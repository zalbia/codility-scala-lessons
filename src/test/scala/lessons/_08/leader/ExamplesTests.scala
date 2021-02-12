package lessons._08.leader
import utest._

import scala.util.Random

object ExamplesTests extends TestSuite {
  val random = new Random()

  import Examples._

  val solutions = Array(slowLeader _, fastLeader _, goldenLeader _, goldenLeaderStack _)

  val tests = Tests {
    test("example") { check(Array(6, 8, 4, 6, 8, 6, 6), 6) }
    test { check(Array(), -1) }
    test { check(Array(1), 1) }
    test { check(Array(0, 0), 0) }
    test { check(Array(0, 1), -1) }
    test { check(Array(1, 0), -1) }
    test { check(Array(0, 1, 0), 0) }
    test { check(Array(0, 1, 1), 1) }
    test { check(Array(1, 0, 1), 1) }
  }

  def check(a: Array[Int], expected: Int): Unit = {
    solutions.foreach { f =>
      val result = f(a)
      assert(result == expected)
    }
  }
}

