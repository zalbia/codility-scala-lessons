package lessons._07.stacks_and_queues._3
import utest._

import scala.util.Random

object NestingTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example1") { check("(()(())())", 1) }
    test("example2") { check("())", 0) }
    test(check("", 1))
    test(check("(", 0))
    test(check(")", 0))
    test(check(")(", 0))
    test(check("()", 1))
    test(check("(()", 0))

    val max = 1000000
    test(check("(" * max, 0))
    test(check(")" * max, 0))
    test(check("()" * (max / 2), 1))
    test(check(")(" * (max / 2), 0))
    test(check("(" * (max / 2) ++ ")" * (max / 2), 1))
  }

  def check(a: String, expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}

