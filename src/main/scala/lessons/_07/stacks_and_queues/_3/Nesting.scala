package lessons._07.stacks_and_queues._3

object Solution {
  // N is an integer within the range [0..1,000,000];
  // string S consists only of the characters "(" and/or ")".
  def solution(s: String): Int = {
    if (s.length % 2 == 1) 0
    else {
      var openCount = 0
      for (i <- s.indices) {
        val paren = s(i)
        if (paren == '(')
          openCount += 1
        else if (openCount == 0)
          return 0
        else
          openCount -= 1
      }
      if (openCount == 0) 1 else 0
    }
  }
}

import utest._

import scala.util.Random

object NestingTests extends TestSuite {
  val random = new Random()
  val f = Solution.solution _

  val tests = Tests {
    test("example1") { check("(()(())())", 1) }
    test("example2") { check("())", 0) }
    test { check("", 1) }
    test { check("(", 0) }
    test { check(")", 0) }
    test { check(")(", 0) }
    test { check("()", 1) }
    test { check("(()", 0) }

    val max = 1000000
    test { check("(" * max, 0) }
    test { check(")" * max, 0) }
    test { check("()" * (max / 2), 1) }
    test { check(")(" * (max / 2), 0) }
    test { check("(" * (max / 2) ++ ")" * (max / 2), 1) }
  }

  def check(a: String, expected: Int): Unit = {
    val result = f(a)
    assert(result == expected)
  }
}

