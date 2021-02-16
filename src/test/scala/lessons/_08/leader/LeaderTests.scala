package lessons._08.leader
import utest._

import scala.util.Random

object LeaderTests extends TestSuite {
  val random = new Random()

  import Leader._

  val solutions = Array(
    slow_leader.array.slowLeader _,
    slow_leader.map.slowLeader _,
    fastLeader _,
    golden_leader.constant_space.goldenLeader _,
    golden_leader.stack.goldenLeader _,
    golden_leader.constant_space.goldenLeader _
  )

  val tests = Tests {
    test("example") { check(Array(6, 8, 4, 6, 8, 6, 6), 6) }
    test(check(Array(), -1))
    test(check(Array(1), 1))
    test(check(Array(0, 0), 0))
    test(check(Array(0, 1), -1))
    test(check(Array(1, 0), -1))
    test(check(Array(0, 1, 0), 0))
    test(check(Array(0, 1, 1), 1))
    test(check(Array(1, 0, 1), 1))
  }

  def check(a: Array[Int], expected: Int): Unit = {
    solutions.foreach { f =>
      val result = f(a)
      assert(result == expected)
    }
  }
}

