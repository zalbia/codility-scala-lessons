package lessons._07.stacks_and_queues._1
import utest._

import scala.util.Random

object BracketsTests extends TestSuite {
  val random = new Random()
  val f      = Solution.solution _

  val tests = Tests {
    test("example-balanced")(check("{[()()]}", 1))
    test("example-imbalanced")(check("([)()]", 0))
    test(check("", 1))
    test(check("{", 0))
    test(check("}", 0))
    test(check("}}}}}", 0))
    test(check("}" * 200000, 0))
    test(check("(" * 200000, 0))
    test(check("()" * 100000, 1))
    test(check("(" * 100000 ++ ")" * 100000, 1))
  }

  def check(a: String, expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}
